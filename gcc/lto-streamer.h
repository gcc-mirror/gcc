/* Data structures and declarations used for reading and writing
   GIMPLE to a file stream.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Doug Kwan <dougkwan@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_LTO_STREAMER_H
#define GCC_LTO_STREAMER_H

#include "plugin-api.h"
#include "tree.h"
#include "gimple.h"
#include "target.h"
#include "cgraph.h"
#include "vec.h"
#include "vecprim.h"
#include "alloc-pool.h"
#include "gcov-io.h"

/* Define when debugging the LTO streamer.  This causes the writer
   to output the numeric value for the memory address of the tree node
   being emitted.  When debugging a problem in the reader, check the
   original address that the writer was emitting using lto_orig_address_get.
   With this value, set a breakpoint in the writer (e.g., lto_output_tree)
   to trace how the faulty node is being emitted.  */
/* #define LTO_STREAMER_DEBUG	1  */

/* The encoding for a function consists of the following sections:

   1)    The header.
   2)    FIELD_DECLS.
   3)    FUNCTION_DECLS.
   4)    global VAR_DECLS.
   5)    type_decls
   6)    types.
   7)    Names for the labels that have names
   8)    The SSA names.
   9)    The control flow graph.
   10-11)Gimple for local decls.
   12)   Gimple for the function.
   13)   Strings.

   1) THE HEADER.
   2-6) THE GLOBAL DECLS AND TYPES.

      The global decls and types are encoded in the same way.  For each
      entry, there is word with the offset within the section to the
      entry.

   7) THE LABEL NAMES.

      Since most labels do not have names, this section my be of zero
      length.  It consists of an array of string table references, one
      per label.  In the lto code, the labels are given either
      positive or negative indexes.  the positive ones have names and
      the negative ones do not.  The positive index can be used to
      find the name in this array.

   9) THE CFG.

   10) Index into the local decls.  Since local decls can have local
      decls inside them, they must be read in randomly in order to
      properly restore them.

   11-12) GIMPLE FOR THE LOCAL DECLS AND THE FUNCTION BODY.

     The gimple consists of a set of records.

     THE FUNCTION

     At the top level of (8) is the function. It consists of five
     pieces:

     LTO_function     - The tag.
     eh tree          - This is all of the exception handling regions
                        put out in a post order traversial of the
                        tree.  Siblings are output as lists terminated
			by a 0.  The set of fields matches the fields
			defined in except.c.

     last_basic_block - in uleb128 form.

     basic blocks     - This is the set of basic blocks.

     zero             - The termination of the basic blocks.

     BASIC BLOCKS

     There are two forms of basic blocks depending on if they are
     empty or not.

     The basic block consists of:

     LTO_bb1 or LTO_bb0 - The tag.

     bb->index          - the index in uleb128 form.

     #succs             - The number of successors un uleb128 form.

     the successors     - For each edge, a pair.  The first of the
                          pair is the index of the successor in
                          uleb128 form and the second are the flags in
                          uleb128 form.

     the statements     - A gimple tree, as described above.
                          These are only present for LTO_BB1.
                          Following each statement is an optional
                          exception handling record LTO_eh_region
			  which contains the region number (for
			  regions >= 0).

     zero               - This is only present for LTO_BB1 and is used
			  to terminate the statements and exception
			  regions within this block.

   12) STRINGS

     String are represented in the table as pairs, a length in ULEB128
     form followed by the data for the string.  */

/* The string that is the prefix on the section names we make for lto.
   For decls the DECL_ASSEMBLER_NAME is appended to make the section
   name for the functions and static_initializers.  For other types of
   sections a '.' and the section type are appended.  */
#define LTO_SECTION_NAME_PREFIX         ".gnu.lto_"

#define LTO_major_version 2
#define LTO_minor_version 0

typedef unsigned char	lto_decl_flags_t;


/* Data structures used to pack values and bitflags into a vector of
   words.  Used to stream values of a fixed number of bits in a space
   efficient way.  */
static unsigned const BITS_PER_BITPACK_WORD = HOST_BITS_PER_WIDE_INT;

typedef unsigned HOST_WIDE_INT bitpack_word_t;
DEF_VEC_I(bitpack_word_t);
DEF_VEC_ALLOC_I(bitpack_word_t, heap);

struct bitpack_d
{
  /* The position of the first unused or unconsumed bit in the word.  */
  unsigned pos;

  /* The current word we are (un)packing.  */
  bitpack_word_t word;

  /* The lto_output_stream or the lto_input_block we are streaming to/from.  */
  void *stream;
};

/* Tags representing the various IL objects written to the bytecode file
   (GIMPLE statements, basic blocks, EH regions, tree nodes, etc).

   NOTE, when adding new LTO tags, also update lto_tag_name.  */
enum LTO_tags
{
  LTO_null = 0,

  /* Reserve enough entries to fit all the tree and gimple codes handled
     by the streamer.  This guarantees that:

     1- Given a tree code C:
     		enum LTO_tags tag == C + 1

     2- Given a gimple code C:
		enum LTO_tags tag == C + NUM_TREE_CODES + 1

     Conversely, to map between LTO tags and tree/gimple codes, the
     reverse operation must be applied.  */
  LTO_bb0 = 1 + NUM_TREE_CODES + LAST_AND_UNUSED_GIMPLE_CODE,
  LTO_bb1,

  /* EH region holding the previous statement.  */
  LTO_eh_region,

  /* An MD or NORMAL builtin.  Only the code and class are streamed out.  */
  LTO_builtin_decl,

  /* Function body.  */
  LTO_function,

  /* EH table.  */
  LTO_eh_table,

  /* EH region types.  These mirror enum eh_region_type.  */
  LTO_ert_cleanup,
  LTO_ert_try,
  LTO_ert_allowed_exceptions,
  LTO_ert_must_not_throw,

  /* EH landing pad.  */
  LTO_eh_landing_pad,

  /* EH try/catch node.  */
  LTO_eh_catch,

  /* Special for global streamer. Reference to previously-streamed node.  */
  LTO_tree_pickle_reference,

  /* References to indexable tree nodes.  These objects are stored in
     tables that are written separately from the function bodies that
     reference them.  This way they can be instantiated even when the
     referencing functions aren't (e.g., during WPA) and it also allows
     functions to be copied from one file to another without having
     to unpickle the body first (the references are location
     independent).

     NOTE, do not regroup these values as the grouping is exposed
     in the range checks done in lto_input_tree.  */
  LTO_field_decl_ref,			/* Do not change.  */
  LTO_function_decl_ref,
  LTO_label_decl_ref,
  LTO_namespace_decl_ref,
  LTO_result_decl_ref,
  LTO_ssa_name_ref,
  LTO_type_decl_ref,
  LTO_type_ref,
  LTO_const_decl_ref,
  LTO_imported_decl_ref,
  LTO_translation_unit_decl_ref,
  LTO_global_decl_ref,			/* Do not change.  */

  /* This tag must always be last.  */
  LTO_NUM_TAGS
};


/* Set of section types that are in an LTO file.  This list will grow
   as the number of IPA passes grows since each IPA pass will need its
   own section type to store its summary information.

   When adding a new section type, you must also extend the
   LTO_SECTION_NAME array in lto-section-in.c.  */
enum lto_section_type
{
  LTO_section_decls = 0,
  LTO_section_function_body,
  LTO_section_static_initializer,
  LTO_section_cgraph,
  LTO_section_varpool,
  LTO_section_refs,
  LTO_section_jump_functions,
  LTO_section_ipa_pure_const,
  LTO_section_ipa_reference,
  LTO_section_symtab,
  LTO_section_opts,
  LTO_section_cgraph_opt_sum,
  LTO_N_SECTION_TYPES		/* Must be last.  */
};

/* Indices to the various function, type and symbol streams. */
typedef enum
{
  LTO_DECL_STREAM_TYPE = 0,		/* Must be first. */
  LTO_DECL_STREAM_FIELD_DECL,
  LTO_DECL_STREAM_FN_DECL,
  LTO_DECL_STREAM_VAR_DECL,
  LTO_DECL_STREAM_TYPE_DECL,
  LTO_DECL_STREAM_NAMESPACE_DECL,
  LTO_DECL_STREAM_LABEL_DECL,
  LTO_N_DECL_STREAMS
} lto_decl_stream_e_t;

typedef enum ld_plugin_symbol_resolution ld_plugin_symbol_resolution_t;
DEF_VEC_I(ld_plugin_symbol_resolution_t);
DEF_VEC_ALLOC_I(ld_plugin_symbol_resolution_t, heap);


/* Macro to define convenience functions for type and decl streams
   in lto_file_decl_data.  */
#define DEFINE_DECL_STREAM_FUNCS(UPPER_NAME, name) \
static inline tree \
lto_file_decl_data_get_ ## name (struct lto_file_decl_data *data, \
				 unsigned int idx) \
{ \
  struct lto_in_decl_state *state = data->current_decl_state; \
  gcc_assert (idx < state->streams[LTO_DECL_STREAM_## UPPER_NAME].size); \
  return state->streams[LTO_DECL_STREAM_## UPPER_NAME].trees[idx]; \
} \
\
static inline unsigned int \
lto_file_decl_data_num_ ## name ## s (struct lto_file_decl_data *data) \
{ \
  struct lto_in_decl_state *state = data->current_decl_state; \
  return state->streams[LTO_DECL_STREAM_## UPPER_NAME].size; \
}


/* Return a char pointer to the start of a data stream for an lto pass
   or function.  The first parameter is the file data that contains
   the information.  The second parameter is the type of information
   to be obtained.  The third parameter is the name of the function
   and is only used when finding a function body; otherwise it is
   NULL.  The fourth parameter is the length of the data returned.  */
typedef const char* (lto_get_section_data_f) (struct lto_file_decl_data *,
					      enum lto_section_type,
					      const char *,
					      size_t *);

/* Return the data found from the above call.  The first three
   parameters are the same as above.  The fourth parameter is the data
   itself and the fifth is the lenght of the data. */
typedef void (lto_free_section_data_f) (struct lto_file_decl_data *,
					enum lto_section_type,
					const char *,
					const char *,
					size_t);

/* Cache of pickled nodes.  Used to avoid writing the same node more
   than once.  The first time a tree node is streamed out, it is
   entered in this cache.  Subsequent references to the same node are
   resolved by looking it up in this cache.

   This is used in two ways:

   - On the writing side, the first time T is added to STREAMER_CACHE,
     a new reference index is created for T and T is emitted on the
     stream.  If T needs to be emitted again to the stream, instead of
     pickling it again, the reference index is emitted.

   - On the reading side, the first time T is read from the stream, it
     is reconstructed in memory and a new reference index created for
     T.  The reconstructed T is inserted in some array so that when
     the reference index for T is found in the input stream, it can be
     used to look up into the array to get the reconstructed T.  */
struct lto_streamer_cache_d
{
  /* The mapping between tree nodes and slots into the nodes array.  */
  htab_t node_map;

  /* Node map to store entries into.  */
  alloc_pool node_map_entries;

  /* Next available slot in the nodes and offsets arrays.  */
  unsigned next_slot;

  /* The nodes pickled so far.  */
  VEC(tree,heap) *nodes;

  /* Offset into the stream where the nodes have been written.  */
  VEC(unsigned,heap) *offsets;
};


/* Structure used as buffer for reading an LTO file.  */
struct lto_input_block
{
  const char *data;
  unsigned int p;
  unsigned int len;
};

#define LTO_INIT_INPUT_BLOCK(BASE,D,P,L)   \
  do {                                     \
    BASE.data = D;                         \
    BASE.p = P;                            \
    BASE.len = L;                          \
  } while (0)

#define LTO_INIT_INPUT_BLOCK_PTR(BASE,D,P,L) \
  do {                                       \
    BASE->data = D;                          \
    BASE->p = P;                             \
    BASE->len = L;                           \
  } while (0)


/* The is the first part of the record for a function or constructor
   in the .o file.  */
struct lto_header
{
  int16_t major_version;
  int16_t minor_version;
  enum lto_section_type section_type;
};

/* The header for a function body.  */
struct lto_function_header
{
  /* The header for all types of sections. */
  struct lto_header lto_header;

  /* Number of labels with names.  */
  int32_t num_named_labels;

  /* Number of labels without names.  */
  int32_t num_unnamed_labels;

  /* Size compressed or 0 if not compressed.  */
  int32_t compressed_size;

  /* Size of names for named labels.  */
  int32_t named_label_size;

  /* Size of the cfg.  */
  int32_t cfg_size;

  /* Size of main gimple body of function.  */
  int32_t main_size;

  /* Size of the string table.  */
  int32_t string_size;
};


/* Structure describing a symbol section.  */
struct lto_decl_header
{
  /* The header for all types of sections. */
  struct lto_header lto_header;

  /* Size of region for decl state. */
  int32_t decl_state_size;

  /* Number of nodes in globals stream.  */
  int32_t num_nodes;

  /* Size of region for expressions, decls, types, etc. */
  int32_t main_size;

  /* Size of the string table.  */
  int32_t string_size;
};


/* Statistics gathered during LTO, WPA and LTRANS.  */
struct lto_stats_d
{
  unsigned HOST_WIDE_INT num_input_cgraph_nodes;
  unsigned HOST_WIDE_INT num_output_cgraph_nodes;
  unsigned HOST_WIDE_INT num_input_files;
  unsigned HOST_WIDE_INT num_output_files;
  unsigned HOST_WIDE_INT num_cgraph_partitions;
  unsigned HOST_WIDE_INT section_size[LTO_N_SECTION_TYPES];
  unsigned HOST_WIDE_INT num_function_bodies;
  unsigned HOST_WIDE_INT num_trees[NUM_TREE_CODES];
  unsigned HOST_WIDE_INT num_output_il_bytes;
  unsigned HOST_WIDE_INT num_compressed_il_bytes;
  unsigned HOST_WIDE_INT num_input_il_bytes;
  unsigned HOST_WIDE_INT num_uncompressed_il_bytes;
};

/* Encoder data structure used to stream callgraph nodes.  */
struct lto_cgraph_encoder_d
{
  /* Map nodes to reference number. */
  struct pointer_map_t *map;

  /* Map reference number to node. */
  VEC(cgraph_node_ptr,heap) *nodes;

  /* Map of nodes where we want to output body.  */
  struct pointer_set_t *body;
};

typedef struct lto_cgraph_encoder_d *lto_cgraph_encoder_t;

/* Return number of encoded nodes in ENCODER.  */

static inline int
lto_cgraph_encoder_size (lto_cgraph_encoder_t encoder)
{
  return VEC_length (cgraph_node_ptr, encoder->nodes);
}


/* Encoder data structure used to stream callgraph nodes.  */
struct lto_varpool_encoder_d
{
  /* Map nodes to reference number. */
  struct pointer_map_t *map;

  /* Map reference number to node. */
  VEC(varpool_node_ptr,heap) *nodes;

  /* Map of nodes where we want to output initializer.  */
  struct pointer_set_t *initializer;
};
typedef struct lto_varpool_encoder_d *lto_varpool_encoder_t;

/* Return number of encoded nodes in ENCODER.  */

static inline int
lto_varpool_encoder_size (lto_varpool_encoder_t encoder)
{
  return VEC_length (varpool_node_ptr, encoder->nodes);
}

/* Mapping from indices to trees.  */
struct GTY(()) lto_tree_ref_table
{
  /* Array of referenced trees . */
  tree * GTY((length ("%h.size"))) trees;

  /* Size of array. */
  unsigned int size;
};


/* Mapping between trees and slots in an array.  */
struct lto_decl_slot
{
  tree t;
  int slot_num;
};


/* The lto_tree_ref_encoder struct is used to encode trees into indices. */

struct lto_tree_ref_encoder
{
  htab_t tree_hash_table;	/* Maps pointers to indices. */
  unsigned int next_index;	/* Next available index. */
  VEC(tree,heap) *trees;	/* Maps indices to pointers. */
};


/* Structure to hold states of input scope.  */
struct GTY(()) lto_in_decl_state
{
  /* Array of lto_in_decl_buffers to store type and decls streams. */
  struct lto_tree_ref_table streams[LTO_N_DECL_STREAMS];

  /* If this in-decl state is associated with a function. FN_DECL
     point to the FUNCTION_DECL. */
  tree fn_decl;
};

typedef struct lto_in_decl_state *lto_in_decl_state_ptr;


/* The structure that holds all of the vectors of global types,
   decls and cgraph nodes used in the serialization of this file.  */
struct lto_out_decl_state
{
  /* The buffers contain the sets of decls of various kinds and types we have
     seen so far and the indexes assigned to them.  */
  struct lto_tree_ref_encoder streams[LTO_N_DECL_STREAMS];

  /* Encoder for cgraph nodes.  */
  lto_cgraph_encoder_t cgraph_node_encoder;

  /* Encoder for varpool nodes.  */
  lto_varpool_encoder_t varpool_node_encoder;

  /* If this out-decl state belongs to a function, fn_decl points to that
     function.  Otherwise, it is NULL. */
  tree fn_decl;
};

typedef struct lto_out_decl_state *lto_out_decl_state_ptr;

DEF_VEC_P(lto_out_decl_state_ptr);
DEF_VEC_ALLOC_P(lto_out_decl_state_ptr, heap);

/* One of these is allocated for each object file that being compiled
   by lto.  This structure contains the tables that are needed by the
   serialized functions and ipa passes to connect themselves to the
   global types and decls as they are reconstituted.  */
struct GTY(()) lto_file_decl_data
{
  /* Decl state currently used. */
  struct lto_in_decl_state *current_decl_state;

  /* Decl state corresponding to regions outside of any functions
     in the compilation unit. */
  struct lto_in_decl_state *global_decl_state;

  /* Table of cgraph nodes present in this file.  */
  lto_cgraph_encoder_t GTY((skip)) cgraph_node_encoder;

  /* Table of varpool nodes present in this file.  */
  lto_varpool_encoder_t GTY((skip)) varpool_node_encoder;

  /* Hash table maps lto-related section names to location in file.  */
  htab_t GTY((param_is (struct lto_in_decl_state))) function_decl_states;

  /* The .o file that these offsets relate to.  */
  const char *GTY((skip)) file_name;

  /* Hash table maps lto-related section names to location in file.  */
  htab_t GTY((skip)) section_hash_table;

  /* Hash new name of renamed global declaration to its original name.  */
  htab_t GTY((skip)) renaming_hash_table;

  /* Linked list used temporarily in reader */
  struct lto_file_decl_data *next;

  /* Sub ID for merged objects. */
  unsigned id;

  /* Symbol resolutions for this file */
  VEC(ld_plugin_symbol_resolution_t,heap) * GTY((skip)) resolutions;

  struct gcov_ctr_summary GTY((skip)) profile_info;
};

typedef struct lto_file_decl_data *lto_file_decl_data_ptr;

struct lto_char_ptr_base
{
  char *ptr;
};

/* An incore byte stream to buffer the various parts of the function.
   The entire structure should be zeroed when created.  The record
   consists of a set of blocks.  The first sizeof (ptr) bytes are used
   as a chain, and the rest store the bytes to be written.  */
struct lto_output_stream
{
  /* The pointer to the first block in the stream.  */
  struct lto_char_ptr_base * first_block;

  /* The pointer to the last and current block in the stream.  */
  struct lto_char_ptr_base * current_block;

  /* The pointer to where the next char should be written.  */
  char * current_pointer;

  /* The number of characters left in the current block.  */
  unsigned int left_in_block;

  /* The block size of the last block allocated.  */
  unsigned int block_size;

  /* The total number of characters written.  */
  unsigned int total_size;
};

/* The is the first part of the record in an LTO file for many of the
   IPA passes.  */
struct lto_simple_header
{
  /* The header for all types of sections. */
  struct lto_header lto_header;

  /* Size of main gimple body of function.  */
  int32_t main_size;

  /* Size of main stream when compressed.  */
  int32_t compressed_size;
};

/* A simple output block.  This can be used for simple IPA passes that
   do not need more than one stream.  */
struct lto_simple_output_block
{
  enum lto_section_type section_type;
  struct lto_out_decl_state *decl_state;

  /* The stream that the main tree codes are written to.  */
  struct lto_output_stream *main_stream;
};

/* Data structure holding all the data and descriptors used when writing
   an LTO file.  */
struct output_block
{
  enum lto_section_type section_type;
  struct lto_out_decl_state *decl_state;

  /* The stream that the main tree codes are written to.  */
  struct lto_output_stream *main_stream;

  /* The stream that contains the string table.  */
  struct lto_output_stream *string_stream;

  /* The stream that contains the cfg.  */
  struct lto_output_stream *cfg_stream;

  /* The hash table that contains the set of strings we have seen so
     far and the indexes assigned to them.  */
  htab_t string_hash_table;

  /* The current cgraph_node that we are currently serializing.  Null
     if we are serializing something else.  */
  struct cgraph_node *cgraph_node;

  /* These are the last file and line that were seen in the stream.
     If the current node differs from these, it needs to insert
     something into the stream and fix these up.  */
  const char *current_file;
  int current_line;
  int current_col;

  /* True if writing globals and types.  */
  bool global;

  /* Cache of nodes written in this section.  */
  struct lto_streamer_cache_d *writer_cache;
};


/* Data and descriptors used when reading from an LTO file.  */
struct data_in
{
  /* The global decls and types.  */
  struct lto_file_decl_data *file_data;

  /* All of the labels.  */
  tree *labels;

  /* The string table.  */
  const char *strings;

  /* The length of the string table.  */
  unsigned int strings_len;

  /* Number of named labels.  Used to find the index of unnamed labels
     since they share space with the named labels.  */
  unsigned int num_named_labels;

  /* Number of unnamed labels.  */
  unsigned int num_unnamed_labels;

  const char *current_file;
  int current_line;
  int current_col;

  /* Maps each reference number to the resolution done by the linker. */
  VEC(ld_plugin_symbol_resolution_t,heap) *globals_resolution;

  /* Cache of pickled nodes.  */
  struct lto_streamer_cache_d *reader_cache;
};


/* In lto-section-in.c  */
extern struct lto_input_block * lto_create_simple_input_block (
			       struct lto_file_decl_data *,
			       enum lto_section_type, const char **, size_t *);
extern void
lto_destroy_simple_input_block (struct lto_file_decl_data *,
				enum lto_section_type,
				struct lto_input_block *, const char *, size_t);
extern void lto_set_in_hooks (struct lto_file_decl_data **,
			      lto_get_section_data_f *,
			      lto_free_section_data_f *);
extern struct lto_file_decl_data **lto_get_file_decl_data (void);
extern const char *lto_get_section_data (struct lto_file_decl_data *,
					 enum lto_section_type,
					 const char *, size_t *);
extern void lto_free_section_data (struct lto_file_decl_data *,
				   enum lto_section_type,
				   const char *, const char *, size_t);
extern unsigned char lto_input_1_unsigned (struct lto_input_block *);
extern unsigned HOST_WIDE_INT lto_input_uleb128 (struct lto_input_block *);
extern unsigned HOST_WIDEST_INT lto_input_widest_uint_uleb128 (
						struct lto_input_block *);
extern HOST_WIDE_INT lto_input_sleb128 (struct lto_input_block *);
extern htab_t lto_create_renaming_table (void);
extern void lto_record_renamed_decl (struct lto_file_decl_data *,
				     const char *, const char *);
extern const char *lto_get_decl_name_mapping (struct lto_file_decl_data *,
					      const char *);
extern struct lto_in_decl_state *lto_new_in_decl_state (void);
extern void lto_delete_in_decl_state (struct lto_in_decl_state *);
extern hashval_t lto_hash_in_decl_state (const void *);
extern int lto_eq_in_decl_state (const void *, const void *);
extern struct lto_in_decl_state *lto_get_function_in_decl_state (
				      struct lto_file_decl_data *, tree);

/* In lto-section-out.c  */
extern hashval_t lto_hash_decl_slot_node (const void *);
extern int lto_eq_decl_slot_node (const void *, const void *);
extern hashval_t lto_hash_type_slot_node (const void *);
extern int lto_eq_type_slot_node (const void *, const void *);
extern void lto_begin_section (const char *, bool);
extern void lto_end_section (void);
extern void lto_write_stream (struct lto_output_stream *);
extern void lto_output_1_stream (struct lto_output_stream *, char);
extern void lto_output_data_stream (struct lto_output_stream *, const void *,
				    size_t);
extern void lto_output_uleb128_stream (struct lto_output_stream *,
       				       unsigned HOST_WIDE_INT);
extern void lto_output_widest_uint_uleb128_stream (struct lto_output_stream *,
       					           unsigned HOST_WIDEST_INT);
extern void lto_output_sleb128_stream (struct lto_output_stream *,
				       HOST_WIDE_INT);
extern bool lto_output_decl_index (struct lto_output_stream *,
			    struct lto_tree_ref_encoder *,
			    tree, unsigned int *);
extern void lto_output_field_decl_index (struct lto_out_decl_state *,
				  struct lto_output_stream *, tree);
extern void lto_output_fn_decl_index (struct lto_out_decl_state *,
			       struct lto_output_stream *, tree);
extern void lto_output_namespace_decl_index (struct lto_out_decl_state *,
				      struct lto_output_stream *, tree);
extern void lto_output_var_decl_index (struct lto_out_decl_state *,
				struct lto_output_stream *, tree);
extern void lto_output_type_decl_index (struct lto_out_decl_state *,
				 struct lto_output_stream *, tree);
extern void lto_output_type_ref_index (struct lto_out_decl_state *,
				struct lto_output_stream *, tree);
extern struct lto_simple_output_block *lto_create_simple_output_block (
				enum lto_section_type);
extern void lto_destroy_simple_output_block (struct lto_simple_output_block *);
extern struct lto_out_decl_state *lto_new_out_decl_state (void);
extern void lto_delete_out_decl_state (struct lto_out_decl_state *);
extern struct lto_out_decl_state *lto_get_out_decl_state (void);
extern void lto_push_out_decl_state (struct lto_out_decl_state *);
extern struct lto_out_decl_state *lto_pop_out_decl_state (void);
extern void lto_record_function_out_decl_state (tree,
						struct lto_out_decl_state *);


/* In lto-streamer.c.  */
extern const char *lto_tag_name (enum LTO_tags);
extern bitmap lto_bitmap_alloc (void);
extern void lto_bitmap_free (bitmap);
extern char *lto_get_section_name (int, const char *, struct lto_file_decl_data *);
extern void print_lto_report (void);
extern bool lto_streamer_cache_insert (struct lto_streamer_cache_d *, tree,
				       int *, unsigned *);
extern bool lto_streamer_cache_insert_at (struct lto_streamer_cache_d *, tree,
					  int);
extern bool lto_streamer_cache_lookup (struct lto_streamer_cache_d *, tree,
				       int *);
extern tree lto_streamer_cache_get (struct lto_streamer_cache_d *, int);
extern struct lto_streamer_cache_d *lto_streamer_cache_create (void);
extern void lto_streamer_cache_delete (struct lto_streamer_cache_d *);
extern void lto_streamer_init (void);
extern bool gate_lto_out (void);
#ifdef LTO_STREAMER_DEBUG
extern void lto_orig_address_map (tree, intptr_t);
extern intptr_t lto_orig_address_get (tree);
extern void lto_orig_address_remove (tree);
#endif
extern void lto_check_version (int, int);


/* In lto-streamer-in.c */
extern void lto_input_cgraph (struct lto_file_decl_data *, const char *);
extern void lto_init_reader (void);
extern tree lto_input_tree (struct lto_input_block *, struct data_in *);
extern void lto_input_function_body (struct lto_file_decl_data *, tree,
				     const char *);
extern void lto_input_constructors_and_inits (struct lto_file_decl_data *,
					      const char *);
extern void lto_init_reader (void);
extern struct data_in *lto_data_in_create (struct lto_file_decl_data *,
				    const char *, unsigned,
				    VEC(ld_plugin_symbol_resolution_t,heap) *);
extern void lto_data_in_delete (struct data_in *);


/* In lto-streamer-out.c  */
extern void lto_register_decl_definition (tree, struct lto_file_decl_data *);
extern struct output_block *create_output_block (enum lto_section_type);
extern void destroy_output_block (struct output_block *);
extern void lto_output_tree (struct output_block *, tree, bool);
extern void produce_asm (struct output_block *ob, tree fn);


/* In lto-cgraph.c  */
struct cgraph_node *lto_cgraph_encoder_deref (lto_cgraph_encoder_t, int);
int lto_cgraph_encoder_lookup (lto_cgraph_encoder_t, struct cgraph_node *);
lto_cgraph_encoder_t lto_cgraph_encoder_new (void);
int lto_cgraph_encoder_encode (lto_cgraph_encoder_t, struct cgraph_node *);
void lto_cgraph_encoder_delete (lto_cgraph_encoder_t);
bool lto_cgraph_encoder_encode_body_p (lto_cgraph_encoder_t,
				       struct cgraph_node *);

bool lto_varpool_encoder_encode_body_p (lto_varpool_encoder_t,
				        struct varpool_node *);
struct varpool_node *lto_varpool_encoder_deref (lto_varpool_encoder_t, int);
int lto_varpool_encoder_lookup (lto_varpool_encoder_t, struct varpool_node *);
lto_varpool_encoder_t lto_varpool_encoder_new (void);
int lto_varpool_encoder_encode (lto_varpool_encoder_t, struct varpool_node *);
void lto_varpool_encoder_delete (lto_varpool_encoder_t);
bool lto_varpool_encoder_encode_initializer_p (lto_varpool_encoder_t,
					       struct varpool_node *);
void output_cgraph (cgraph_node_set, varpool_node_set);
void input_cgraph (void);
bool referenced_from_other_partition_p (struct ipa_ref_list *,
				        cgraph_node_set,
				        varpool_node_set vset);
bool reachable_from_other_partition_p (struct cgraph_node *,
				       cgraph_node_set);
bool referenced_from_this_partition_p (struct ipa_ref_list *,
				        cgraph_node_set,
				        varpool_node_set vset);
bool reachable_from_this_partition_p (struct cgraph_node *,
				       cgraph_node_set);
void compute_ltrans_boundary (struct lto_out_decl_state *state,
			      cgraph_node_set, varpool_node_set);


/* In lto-symtab.c.  */
extern void lto_symtab_register_decl (tree, ld_plugin_symbol_resolution_t,
				      struct lto_file_decl_data *);
extern void lto_symtab_merge_decls (void);
extern void lto_symtab_merge_cgraph_nodes (void);
extern tree lto_symtab_prevailing_decl (tree decl);
extern enum ld_plugin_symbol_resolution lto_symtab_get_resolution (tree decl);
extern void lto_symtab_free (void);
extern GTY(()) VEC(tree,gc) *lto_global_var_decls;


/* In lto-opts.c.  */
extern void lto_register_user_option (size_t, const char *, int, int);
extern void lto_read_file_options (struct lto_file_decl_data *);
extern void lto_write_options (void);
extern void lto_reissue_options (void);
void lto_clear_user_options (void);
void lto_clear_file_options (void);


/* In lto-wpa-fixup.c  */
void lto_mark_nothrow_fndecl (tree);
void lto_fixup_nothrow_decls (void);


/* Statistics gathered during LTO, WPA and LTRANS.  */
extern struct lto_stats_d lto_stats;

/* Section names corresponding to the values of enum lto_section_type.  */
extern const char *lto_section_name[];

/* Holds all the out decl states of functions output so far in the
   current output file.  */
extern VEC(lto_out_decl_state_ptr, heap) *lto_function_decl_states;

/* Return true if LTO tag TAG corresponds to a tree code.  */
static inline bool
lto_tag_is_tree_code_p (enum LTO_tags tag)
{
  return tag > LTO_null && (unsigned) tag <= NUM_TREE_CODES;
}


/* Return true if LTO tag TAG corresponds to a gimple code.  */
static inline bool
lto_tag_is_gimple_code_p (enum LTO_tags tag)
{
  return (unsigned) tag >= NUM_TREE_CODES + 1
	 && (unsigned) tag < 1 + NUM_TREE_CODES + LAST_AND_UNUSED_GIMPLE_CODE;
}


/* Return the LTO tag corresponding to gimple code CODE.  See enum
   LTO_tags for details on the conversion.  */
static inline enum LTO_tags
lto_gimple_code_to_tag (enum gimple_code code)
{
  return (enum LTO_tags) ((unsigned) code + NUM_TREE_CODES + 1);
}


/* Return the GIMPLE code corresponding to TAG.  See enum LTO_tags for
   details on the conversion.  */
static inline enum gimple_code
lto_tag_to_gimple_code (enum LTO_tags tag)
{
  gcc_assert (lto_tag_is_gimple_code_p (tag));
  return (enum gimple_code) ((unsigned) tag - NUM_TREE_CODES - 1);
}


/* Return the LTO tag corresponding to tree code CODE.  See enum
   LTO_tags for details on the conversion.  */
static inline enum LTO_tags
lto_tree_code_to_tag (enum tree_code code)
{
  return (enum LTO_tags) ((unsigned) code + 1);
}


/* Return the tree code corresponding to TAG.  See enum LTO_tags for
   details on the conversion.  */
static inline enum tree_code
lto_tag_to_tree_code (enum LTO_tags tag)
{
  gcc_assert (lto_tag_is_tree_code_p (tag));
  return (enum tree_code) ((unsigned) tag - 1);
}

/* Initialize an lto_out_decl_buffer ENCODER.  */
static inline void
lto_init_tree_ref_encoder (struct lto_tree_ref_encoder *encoder,
			   htab_hash hash_fn, htab_eq eq_fn)
{
  encoder->tree_hash_table = htab_create (37, hash_fn, eq_fn, free);
  encoder->next_index = 0;
  encoder->trees = NULL;
}


/* Destory an lto_tree_ref_encoder ENCODER by freeing its contents.  The
   memory used by ENCODER is not freed by this function.  */
static inline void
lto_destroy_tree_ref_encoder (struct lto_tree_ref_encoder *encoder)
{
  /* Hash table may be delete already.  */
  if (encoder->tree_hash_table)
    htab_delete (encoder->tree_hash_table);
  VEC_free (tree, heap, encoder->trees);
}

/* Return the number of trees encoded in ENCODER. */
static inline unsigned int
lto_tree_ref_encoder_size (struct lto_tree_ref_encoder *encoder)
{
  return VEC_length (tree, encoder->trees);
}

/* Return the IDX-th tree in ENCODER. */
static inline tree
lto_tree_ref_encoder_get_tree (struct lto_tree_ref_encoder *encoder,
			       unsigned int idx)
{
  return VEC_index (tree, encoder->trees, idx);
}


/* Return true if LABEL should be emitted in the global context.  */
static inline bool
emit_label_in_global_context_p (tree label)
{
  return DECL_NONLOCAL (label) || FORCED_LABEL (label);
}

/* Return true if tree node EXPR should be streamed as a builtin.  For
   these nodes, we just emit the class and function code.  */
static inline bool
lto_stream_as_builtin_p (tree expr)
{
  return (TREE_CODE (expr) == FUNCTION_DECL
	  && DECL_IS_BUILTIN (expr)
	  && (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_NORMAL
	      || DECL_BUILT_IN_CLASS (expr) == BUILT_IN_MD));
}

/* Return true if EXPR is a tree node that can be written to disk.  */
static inline bool
lto_is_streamable (tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  /* Notice that we reject SSA_NAMEs as well.  We only emit the SSA
     name version in lto_output_tree_ref (see output_ssa_names).  */
  return !is_lang_specific (expr)
	 && code != SSA_NAME
	 && code != CALL_EXPR
	 && code != LANG_TYPE
	 && code != MODIFY_EXPR
	 && code != INIT_EXPR
	 && code != TARGET_EXPR
	 && code != BIND_EXPR
	 && code != WITH_CLEANUP_EXPR
	 && code != STATEMENT_LIST
	 && (code == CASE_LABEL_EXPR
	     || code == DECL_EXPR
	     || TREE_CODE_CLASS (code) != tcc_statement);
}

DEFINE_DECL_STREAM_FUNCS (TYPE, type)
DEFINE_DECL_STREAM_FUNCS (FIELD_DECL, field_decl)
DEFINE_DECL_STREAM_FUNCS (FN_DECL, fn_decl)
DEFINE_DECL_STREAM_FUNCS (VAR_DECL, var_decl)
DEFINE_DECL_STREAM_FUNCS (TYPE_DECL, type_decl)
DEFINE_DECL_STREAM_FUNCS (NAMESPACE_DECL, namespace_decl)
DEFINE_DECL_STREAM_FUNCS (LABEL_DECL, label_decl)

/* Returns a new bit-packing context for bit-packing into S.  */
static inline struct bitpack_d
bitpack_create (struct lto_output_stream *s)
{
  struct bitpack_d bp;
  bp.pos = 0;
  bp.word = 0;
  bp.stream = (void *)s;
  return bp;
}

/* Pack the NBITS bit sized value VAL into the bit-packing context BP.  */
static inline void
bp_pack_value (struct bitpack_d *bp, bitpack_word_t val, unsigned nbits)
{
  bitpack_word_t word = bp->word;
  int pos = bp->pos;
  /* If val does not fit into the current bitpack word switch to the
     next one.  */
  if (pos + nbits > BITS_PER_BITPACK_WORD)
    {
      lto_output_uleb128_stream ((struct lto_output_stream *) bp->stream, word);
      word = val;
      pos = nbits;
    }
  else
    {
      word |= val << pos;
      pos += nbits;
    }
  bp->word = word;
  bp->pos = pos;
}

/* Finishes bit-packing of BP.  */
static inline void
lto_output_bitpack (struct bitpack_d *bp)
{
  lto_output_uleb128_stream ((struct lto_output_stream *) bp->stream,
			     bp->word);
  bp->word = 0;
  bp->pos = 0;
}

/* Returns a new bit-packing context for bit-unpacking from IB.  */
static inline struct bitpack_d
lto_input_bitpack (struct lto_input_block *ib)
{
  struct bitpack_d bp;
  bp.word = lto_input_uleb128 (ib);
  bp.pos = 0;
  bp.stream = (void *)ib;
  return bp;
}

/* Unpacks NBITS bits from the bit-packing context BP and returns them.  */
static inline bitpack_word_t
bp_unpack_value (struct bitpack_d *bp, unsigned nbits)
{
  bitpack_word_t mask, val;
  int pos = bp->pos;

  mask = (nbits == BITS_PER_BITPACK_WORD
	  ? (bitpack_word_t) -1
	  : ((bitpack_word_t) 1 << nbits) - 1);

  /* If there are not continuous nbits in the current bitpack word
     switch to the next one.  */
  if (pos + nbits > BITS_PER_BITPACK_WORD)
    {
      bp->word = val = lto_input_uleb128 ((struct lto_input_block *)bp->stream);
      bp->pos = nbits;
      return val & mask;
    }
  val = bp->word;
  val >>= pos;
  bp->pos = pos + nbits;

  return val & mask;
}

#endif /* GCC_LTO_STREAMER_H  */
