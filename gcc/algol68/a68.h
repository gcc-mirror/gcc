/* Definitions for the Algol 68 GCC front end.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef __A68_H__
#define __A68_H__

/* Some common definitions first.  */

#define BUFFER_SIZE 1024
#define SMALL_BUFFER_SIZE 128
#define SNPRINTF_SIZE ((size_t) (BUFFER_SIZE - 1))
#define BUFCLR(z) {memset ((z), 0, BUFFER_SIZE + 1);}
#define MOID_ERROR_WIDTH 80

#define MONADS "%^&+-~!?"
#define NOMADS "></=*"

/* Macro to easily add gcc_tdiag attributes to functions for builds in which
   the compiler supports them.  */

#if (CHECKING_P && GCC_VERSION >= 4001) || GCC_VERSION == BUILDING_GCC_VERSION
#define ATTRIBUTE_A68_DIAG(m, n) __attribute__ ((__format__ (__gcc_tdiag__, m, n))) ATTRIBUTE_NONNULL(m)
#else
#define ATTRIBUTE_A68_DIAG(m, n) ATTRIBUTE_NONNULL(m)
#endif

/* Maximum number of priorities supported for operators.  The Algol 68 RR
   specifies 9.  */

#define MAX_PRIORITY 9

/* The primal scope is the top-level scope.  */

#define PRIMAL_SCOPE 0

/* Deflexing strategy.  See the moid checking routines in
   a68-parser-moids-check for an explanation of these values.  */

enum
{
  NO_DEFLEXING = 1, SAFE_DEFLEXING, ALIAS_DEFLEXING, FORCE_DEFLEXING,
  SKIP_DEFLEXING
};

/* Magic number for the exports data.  */

#define A68_EXPORT_MAGIC_LEN 2

#define A68_EXPORT_MAGIC1 0x0a
#define A68_EXPORT_MAGIC2 0x68

/* The segment name we pass to simple_object_start_read to find Algol 68 export
   data.  */

#ifndef A68_EXPORT_SEGMENT_NAME
#define A68_EXPORT_SEGMENT_NAME "__GNU_A68"
#endif

/* The section name we use when reading and writing export data.  */

#ifndef A68_EXPORT_SECTION_NAME
#define A68_EXPORT_SECTION_NAME ".a68_exports"
#endif

/* ga68 export format definitions.  See ga68-exports.pk.  */

#define GA68_EXPORTS_VERSION 1

#define GA68_MODE_UNKNOWN 0
#define GA68_MODE_VOID    1
#define GA68_MODE_INT     2
#define GA68_MODE_REAL    3
#define GA68_MODE_BITS    4
#define GA68_MODE_BYTES   5
#define GA68_MODE_CHAR    6
#define GA68_MODE_BOOL    7
#define GA68_MODE_CMPL    8
#define GA68_MODE_ROW     9
#define GA68_MODE_STRUCT 10
#define GA68_MODE_UNION  11
#define GA68_MODE_NAME   12
#define GA68_MODE_PROC   13
#define GA68_MODE_STRING 14
#define GA68_MODE_FLEX   15

#define GA68_EXTRACT_MODU 0
#define GA68_EXTRACT_IDEN 1
#define GA68_EXTRACT_MODE 2
#define GA68_EXTRACT_PRIO 3
#define GA68_EXTRACT_OPER 4

/* Then the types.  */

#include "a68-types.h"

/* Front-end global state.  */

extern GTY(()) A68_T a68_common;
#define A68(z)             (a68_common.z)
#define A68_JOB            A68 (job)
#define A68_STANDENV       A68 (standenv)
#define A68_MCACHE(z)      A68 (mode_cache.z)
#define A68_INCLUDE_PATHS  A68 (include_paths)
#define A68_IMPORT_PATHS   A68 (import_paths)
#define A68_MODULE_FILES   A68 (module_files)
#define A68_GLOBAL_TREES   A68 (global_trees)
#define A68_PARSER(Z)      (A68 (parser_state).Z)
#define A68_MODULE_DEFINITION_DECLS   A68 (module_definition_decls)
#define A68_GLOBAL_CONTEXT      A68 (global_context)
#define A68_GLOBAL_DECLARATIONS A68 (global_declarations)

/* Particular pre-defined modes.  */

#define MODE(p)        A68 (a68_modes.p)
#define M_BITS (MODE (BITS))
#define M_BOOL (MODE (BOOL))
#define M_BYTES (MODE (BYTES))
#define M_CHANNEL (MODE (CHANNEL))
#define M_CHAR (MODE (CHAR))
#define M_COLLITEM (MODE (COLLITEM))
#define M_COMPLEX (MODE (COMPLEX))
#define M_C_STRING (MODE (C_STRING))
#define M_ERROR (MODE (ERROR))
#define M_FILE (MODE (FILE))
#define M_FLEX_ROW_BOOL (MODE (FLEX_ROW_BOOL))
#define M_FLEX_ROW_CHAR (MODE (FLEX_ROW_CHAR))
#define M_FORMAT (MODE (FORMAT))
#define M_HIP (MODE (HIP))
#define M_INT (MODE (INT))
#define M_LONG_BITS (MODE (LONG_BITS))
#define M_LONG_BYTES (MODE (LONG_BYTES))
#define M_LONG_COMPLEX (MODE (LONG_COMPLEX))
#define M_LONG_INT (MODE (LONG_INT))
#define M_LONG_LONG_INT (MODE (LONG_LONG_INT))
#define M_LONG_LONG_BITS (MODE (LONG_LONG_BITS))
#define M_LONG_LONG_COMPLEX (MODE (LONG_LONG_COMPLEX))
#define M_LONG_LONG_INT (MODE (LONG_LONG_INT))
#define M_LONG_LONG_REAL (MODE (LONG_LONG_REAL))
#define M_LONG_REAL (MODE (LONG_REAL))
#define M_NIL (MODE (NIL))
#define M_NUMBER (MODE (NUMBER))
#define M_PROC_LONG_REAL_LONG_REAL (MODE (PROC_LONG_REAL_LONG_REAL))
#define M_PROC_REAL_REAL (MODE (PROC_REAL_REAL))
#define M_PROC_REF_FILE_BOOL (MODE (PROC_REF_FILE_BOOL))
#define M_PROC_REF_FILE_VOID (MODE (PROC_REF_FILE_VOID))
#define M_PROC_ROW_CHAR (MODE (PROC_ROW_CHAR))
#define M_PROC_STRING (MODE (PROC_STRING))
#define M_PROC_VOID (MODE (PROC_VOID))
#define M_REAL (MODE (REAL))
#define M_REF_BITS (MODE (REF_BITS))
#define M_REF_BOOL (MODE (REF_BOOL))
#define M_REF_BYTES (MODE (REF_BYTES))
#define M_REF_CHAR (MODE (REF_CHAR))
#define M_REF_COMPLEX (MODE (REF_COMPLEX))
#define M_REF_FILE (MODE (REF_FILE))
#define M_REF_INT (MODE (REF_INT))
#define M_REF_LONG_BITS (MODE (REF_LONG_BITS))
#define M_REF_LONG_BYTES (MODE (REF_LONG_BYTES))
#define M_REF_LONG_COMPLEX (MODE (REF_LONG_COMPLEX))
#define M_REF_LONG_INT (MODE (REF_LONG_INT))
#define M_REF_LONG_LONG_BITS (MODE (REF_LONG_LONG_BITS))
#define M_REF_LONG_LONG_COMPLEX (MODE (REF_LONG_LONG_COMPLEX))
#define M_REF_LONG_LONG_INT (MODE (REF_LONG_LONG_INT))
#define M_REF_LONG_LONG_REAL (MODE (REF_LONG_LONG_REAL))
#define M_REF_LONG_REAL (MODE (REF_LONG_REAL))
#define M_REF_REAL (MODE (REF_REAL))
#define M_REF_REF_FILE (MODE (REF_REF_FILE))
#define M_REF_SHORT_BITS (MODE (REF_SHORT_BITS))
#define M_REF_SHORT_SHORT_BITS (MODE (REF_SHORT_SHORT_BITS))
#define M_REF_ROW_CHAR (MODE (REF_ROW_CHAR))
#define M_REF_ROW_COMPLEX (MODE (REF_ROW_COMPLEX))
#define M_REF_ROW_INT (MODE (REF_ROW_INT))
#define M_REF_ROW_REAL (MODE (REF_ROW_REAL))
#define M_REF_ROW_ROW_COMPLEX (MODE (REF_ROW_ROW_COMPLEX))
#define M_REF_ROW_ROW_REAL (MODE (REF_ROW_ROW_REAL))
#define M_REF_SHORT_INT (MODE (REF_SHORT_INT))
#define M_REF_SHORT_SHORT_INT (MODE (REF_SHORT_SHORT_INT))
#define M_REF_STRING (MODE (REF_STRING))
#define M_ROW_BITS (MODE (ROW_BITS))
#define M_ROW_BOOL (MODE (ROW_BOOL))
#define M_ROW_CHAR (MODE (ROW_CHAR))
#define M_ROW_COMPLEX (MODE (ROW_COMPLEX))
#define M_ROW_INT (MODE (ROW_INT))
#define M_ROW_REAL (MODE (ROW_REAL))
#define M_ROW_ROW_CHAR (MODE (ROW_ROW_CHAR))
#define M_ROW_ROW_COMPLEX (MODE (ROW_ROW_COMPLEX))
#define M_ROW_ROW_REAL (MODE (ROW_ROW_REAL))
#define M_ROW_SIMPLIN (MODE (ROW_SIMPLIN))
#define M_ROW_SIMPLOUT (MODE (ROW_SIMPLOUT))
#define M_ROWS (MODE (ROWS))
#define M_ROW_STRING (MODE (ROW_STRING))
#define M_SEMA (MODE (SEMA))
#define M_SHORT_BITS (MODE (SHORT_BITS))
#define M_SHORT_SHORT_BITS (MODE (SHORT_SHORT_BITS))
#define M_SHORT_INT (MODE (SHORT_INT))
#define M_SHORT_SHORT_INT (MODE (SHORT_SHORT_INT))
#define M_SIMPLIN (MODE (SIMPLIN))
#define M_SIMPLOUT (MODE (SIMPLOUT))
#define M_STRING (MODE (STRING))
#define M_UNDEFINED (MODE (UNDEFINED))
#define M_VACUUM (MODE (VACUUM))
#define M_VOID (MODE (VOID))

/* Usage of TYPE_LANG_FLAG_* flags.  */

#define A68_ROW_TYPE_P(NODE) TYPE_LANG_FLAG_0 (NODE)
#define A68_UNION_TYPE_P(NODE) TYPE_LANG_FLAG_1 (NODE)
#define A68_STRUCT_TYPE_P(NODE) TYPE_LANG_FLAG_2 (NODE)
#define A68_ROWS_TYPE_P(NODE) TYPE_LANG_FLAG_3 (NODE)
#define A68_TYPE_HAS_ROWS_P(NODE) TYPE_LANG_FLAG_4 (NODE)

/* Language-specific tree checkers.  */

#define STRUCT_OR_UNION_TYPE_CHECK(NODE) \
  TREE_CHECK2 (NODE, RECORD_TYPE, UNION_TYPE)

/* Usage of TYPE_LANG_SLOT_* fields.  */

#define TYPE_FORWARD_REFERENCES(NODE) \
  (TYPE_LANG_SLOT_1 (STRUCT_OR_UNION_TYPE_CHECK (NODE)))

/* a68-unistr.cc */

int a68_u8_mbtouc (uint32_t *puc, const uint8_t *s, size_t n);
int a68_u8_uctomb (uint8_t *s, uint32_t uc, ptrdiff_t n);

uint32_t *a68_u8_to_u32 (const uint8_t *s, size_t n, uint32_t *resultbuf, size_t *lengthp);

/* a68-lang.cc */

/* Global types.  These are built in a68_build_a68_type_nodes and used by the
   lowering routines.  */

#define a68_void_type               A68_GLOBAL_TREES[ATI_VOID_TYPE]
#define a68_bool_type               A68_GLOBAL_TREES[ATI_BOOL_TYPE]
#define a68_char_type               A68_GLOBAL_TREES[ATI_CHAR_TYPE]
#define a68_short_short_bits_type   A68_GLOBAL_TREES[ATI_SHORT_SHORT_BITS_TYPE]
#define a68_short_bits_type         A68_GLOBAL_TREES[ATI_SHORT_BITS_TYPE]
#define a68_bits_type               A68_GLOBAL_TREES[ATI_BITS_TYPE]
#define a68_long_bits_type          A68_GLOBAL_TREES[ATI_LONG_BITS_TYPE]
#define a68_long_long_bits_type     A68_GLOBAL_TREES[ATI_LONG_LONG_BITS_TYPE]
#define a68_bytes_type              A68_GLOBAL_TREES[ATI_BYTES_TYPE]
#define a68_long_bytes_type         A68_GLOBAL_TREES[ATI_LONG_BYTES_TYPE]
#define a68_short_short_int_type    A68_GLOBAL_TREES[ATI_SHORT_SHORT_INT_TYPE]
#define a68_short_int_type          A68_GLOBAL_TREES[ATI_SHORT_INT_TYPE]
#define a68_int_type                A68_GLOBAL_TREES[ATI_INT_TYPE]
#define a68_long_int_type           A68_GLOBAL_TREES[ATI_LONG_INT_TYPE]
#define a68_long_long_int_type      A68_GLOBAL_TREES[ATI_LONG_LONG_INT_TYPE]
#define a68_real_type               A68_GLOBAL_TREES[ATI_REAL_TYPE]
#define a68_long_real_type          A68_GLOBAL_TREES[ATI_LONG_REAL_TYPE]
#define a68_long_long_real_type     A68_GLOBAL_TREES[ATI_LONG_LONG_REAL_TYPE]

struct lang_type *a68_build_lang_type (MOID_T *moid);
struct lang_decl *a68_build_lang_decl (NODE_T *node);
MOID_T *a68_type_moid (tree type);

/* a68-diagnostics.cc  */

void a68_error (NODE_T *p, const char *loc_str, ...);
void a68_error_in_pragmat (NODE_T *p, size_t off,
			   const char *loc_str, ...);
bool a68_warning (NODE_T *p, int opt, const char *loc_str, ...);
void a68_inform (NODE_T *p, const char *loc_str, ...);
void a68_fatal (NODE_T *p, const char *loc_str, ...);
void a68_scan_error (LINE_T *u, char *v, const char *txt, ...);

/* a68-parser-scanner.cc  */

bool a68_lexical_analyser (const char *filename, bool *empty_file);
ssize_t a68_file_size (int fd);
ssize_t a68_file_read (int fd, void *buf, size_t n);

/* a68-parser.cc  */

int a68_count_operands (NODE_T *p);
int a68_count_formal_bounds (NODE_T *p);
void a68_count_pictures (NODE_T *p, int *k);
bool a68_is_ref_refety_flex (MOID_T *m);
bool a68_is_semicolon_less (NODE_T *p);
bool a68_is_formal_bounds (NODE_T *p);
bool a68_is_unit_terminator (NODE_T *p);
bool a68_is_loop_keyword (NODE_T *p);
bool a68_is_new_lexical_level (NODE_T *p);
bool a68_dont_mark_here (NODE_T *p);
enum a68_attribute a68_get_good_attribute (NODE_T *p);
void a68_parser (const char *filename);
NODE_INFO_T *a68_new_node_info (void);
GINFO_T *a68_new_genie_info (void);
NODE_T *a68_new_node (void);
NODE_T *a68_some_node (const char *t);
TABLE_T *a68_new_symbol_table (TABLE_T *p);
MOID_T *a68_new_moid (void);
PACK_T *a68_new_pack (void);
TAG_T *a68_new_tag (void);
void a68_make_special_mode (MOID_T **, int m);
void a68_make_sub (NODE_T *p, NODE_T *, enum a68_attribute t);
bool a68_whether (NODE_T *, ...);
bool a68_is_one_of (NODE_T *p, ...);
void a68_bufcat (char *dst, const char *src, int len);
void a68_bufcpy (char *dst, const char *src, int len);
char *a68_new_string (const char *t, ...);
const char *a68_attribute_name (enum a68_attribute attr);
location_t a68_get_node_location (NODE_T *p);
location_t a68_get_line_location (LINE_T *line, const char *pos);

/* a68-parser-top-down.cc  */

void a68_substitute_brackets (NODE_T *p);
const char *a68_phrase_to_text (NODE_T *p, NODE_T **w);
void a68_top_down_parser (NODE_T *p);

/* a68-parser-bottom-up.cc  */

void a68_bottom_up_parser (NODE_T *p);
void a68_bottom_up_error_check (NODE_T *p);
void a68_rearrange_goto_less_jumps (NODE_T *p);
void a68_bottom_up_coalesce_pub (NODE_T *p);

/* a68-parser-extract.cc  */

void a68_extract_indicants (NODE_T *p);
void a68_extract_priorities (NODE_T *p);
void a68_extract_operators (NODE_T *p);
void a68_extract_labels (NODE_T *p, int expect);
void a68_extract_declarations (NODE_T *p);
void a68_elaborate_bold_tags (NODE_T *p);

/* a68-parser-keywords.cc  */

const char *a68_strop_keyword (const char *keyword);
void a68_set_up_tables (void);
TOKEN_T *a68_add_token (TOKEN_T **p, const char *t);
KEYWORD_T *a68_find_keyword (KEYWORD_T *p, const char *t);
KEYWORD_T *a68_find_keyword_from_attribute (KEYWORD_T *p, enum a68_attribute a);

/* a68-postulates.cc  */

void a68_init_postulates (void);
void a68_free_postulate_list (POSTULATE_T *, POSTULATE_T *);
void a68_make_postulate (POSTULATE_T **, MOID_T *, MOID_T *);
POSTULATE_T *a68_is_postulated (POSTULATE_T *, MOID_T *);
POSTULATE_T *a68_is_postulated_pair (POSTULATE_T *, MOID_T *, MOID_T *);

/* a68-parser-moids-check.cc  */

void a68_mode_checker (NODE_T *p);

/* a68-parser-moids-coerce.cc */

void a68_coercion_inserter (NODE_T *p);

/* a68-parser-moids-equivalence.cc  */

bool a68_prove_moid_equivalence (MOID_T *, MOID_T *);

/* a68-parser-brackets.cc  */

void a68_check_parenthesis (NODE_T *top);

/* a68-parser-prelude.cc  */

void a68_make_standard_environ (void);

/* a68-parser-taxes.cc  */

void a68_set_proc_level (NODE_T *p, int n);
void a68_set_nest (NODE_T *p, NODE_T *s);
int a68_first_tag_global (TABLE_T * table, const char *name);
void a68_collect_taxes (NODE_T *p);
TAG_T *a68_add_tag (TABLE_T *s, int a, NODE_T *n, MOID_T *m, int p);
TAG_T *a68_find_tag_global (TABLE_T *table, int a, const char *name);
int a68_is_identifier_or_label_global (TABLE_T *table, const char *name);
void a68_reset_symbol_table_nest_count (NODE_T *p);
void a68_bind_routine_tags_to_tree (NODE_T *p);
void a68_fill_symbol_table_outer (NODE_T *p, TABLE_T *s);
void a68_finalise_symbol_table_setup (NODE_T *p, int l);
void a68_preliminary_symbol_table_setup (NODE_T *p);
void a68_mark_moids (NODE_T *p);
void a68_mark_auxilliary (NODE_T *p);
void a68_warn_for_unused_tags (NODE_T *p);
void a68_jumps_from_procs (NODE_T *p);

/* a68-parser-victal.cc  */

void a68_victal_checker (NODE_T *p);

/* a68-parser-modes.cc  */

int a68_count_pack_members (PACK_T *u);
MOID_T *a68_register_extra_mode (MOID_T **z, MOID_T *u);
MOID_T *a68_create_mode (int att, int dim, NODE_T *node, MOID_T *sub, PACK_T *pack);
MOID_T *a68_search_equivalent_mode (MOID_T *m);
MOID_T *a68_add_mode (MOID_T **z, int att, int dim, NODE_T *node, MOID_T *sub, PACK_T *pack);
void a68_contract_union (MOID_T *u);
PACK_T *a68_absorb_union_pack (PACK_T * u);
void a68_add_mode_to_pack (PACK_T **p, MOID_T *m, const char *text, NODE_T *node);
void a68_add_mode_to_pack_end (PACK_T **p, MOID_T *m, const char *text, NODE_T *node);
void a68_make_moid_list (MODULE_T *mod);

void a68_renumber_moids (MOID_T *p, int n);

/* a68-moids-to-string.cc  */

const char *a68_moid_to_string (MOID_T *n, size_t w, NODE_T *idf,
				bool indicant_value = false);

/* a68-moids-misc.cc  */

bool a68_basic_coercions (MOID_T *p, MOID_T *q, int c, int deflex);
bool a68_clause_allows_balancing (int att);
bool a68_is_balanced (NODE_T *n, SOID_T *y, int sort);
bool a68_is_coercible_in_context (SOID_T *p, SOID_T *q, int deflex);
bool a68_is_coercible (MOID_T *p, MOID_T *q, int c, int deflex);
bool a68_is_coercible_series (MOID_T *p, MOID_T *q, int c, int deflex);
bool a68_is_coercible_stowed (MOID_T *p, MOID_T *q, int c, int deflex);
bool a68_is_deprefable (MOID_T *p);
bool a68_is_equal_modes (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_firmly_coercible (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_firm (MOID_T *p, MOID_T *q);
bool a68_is_meekly_coercible (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_mode_isnt_well (MOID_T *p);
bool a68_is_moid_in_pack (MOID_T *u, PACK_T *v, int deflex);
bool a68_is_name_struct (MOID_T *p);
bool a68_is_nonproc (MOID_T *p);
bool a68_is_printable_mode (MOID_T *p);
bool a68_is_proc_ref_file_void_or_format (MOID_T *p);
bool a68_is_readable_mode (MOID_T *p);
bool a68_is_ref_row (MOID_T *p);
bool a68_is_rows_type (MOID_T *p);
bool a68_is_softly_coercible (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_strongly_coercible (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_strong_name (MOID_T *p, MOID_T *q);
bool a68_is_strong_slice (MOID_T *p, MOID_T *q);
bool a68_is_subset (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_transput_mode (MOID_T *p, char rw);
bool a68_is_unitable (MOID_T *p, MOID_T *q, int deflex);
bool a68_is_weakly_coercible (MOID_T * p, MOID_T * q, int deflex);
bool a68_is_widenable (MOID_T *p, MOID_T *q);
MOID_T *a68_absorb_related_subsets (MOID_T *m);
MOID_T *a68_depref_completely (MOID_T *p);
MOID_T *a68_depref_once (MOID_T *p);
MOID_T *a68_depref_rows (MOID_T *p, MOID_T *q);
MOID_T *a68_deproc_completely (MOID_T *p);
MOID_T *a68_derow (MOID_T *p);
MOID_T *a68_determine_unique_mode (SOID_T *z, int deflex);
MOID_T *a68_get_balanced_mode (MOID_T *m, int sort, bool return_depreffed, int deflex);
MOID_T *a68_get_balanced_mode_or_no_mode (MOID_T *m, int sort, bool return_depreffed, int deflex);
MOID_T *a68_make_series_from_moids (MOID_T *u, MOID_T *v);
MOID_T *a68_make_united_mode (MOID_T *m);
MOID_T *a68_pack_soids_in_moid (SOID_T *top_sl, int attribute);
MOID_T *a68_unites_to (MOID_T *m, MOID_T *u);
void a68_absorb_series_pack (MOID_T **p);
void a68_absorb_series_union_pack (MOID_T **p);
void a68_add_to_soid_list (SOID_T **root, NODE_T *where, SOID_T *soid);
void a68_free_soid_list (SOID_T *root);
void a68_investigate_firm_relations (PACK_T *u, PACK_T *v, bool *all, bool *some);
void a68_make_coercion (NODE_T *l, enum a68_attribute a, MOID_T *m);
void a68_make_depreffing_coercion (NODE_T *n, MOID_T *p, MOID_T *q);
void a68_make_ref_rowing_coercion (NODE_T *n, MOID_T *p, MOID_T *q);
void a68_make_rowing_coercion (NODE_T *n, MOID_T *p, MOID_T *q);
void a68_make_soid (SOID_T *s, int sort, MOID_T *type, int attribute);
void a68_make_strong (NODE_T *n, MOID_T *p, MOID_T *q);
void a68_make_uniting_coercion (NODE_T *n, MOID_T *q);
void a68_make_void (NODE_T *p, MOID_T *q);

#define A68_DEPREF true
#define A68_NO_DEPREF false

#define A68_IF_MODE_IS_WELL(n) (! ((n) == M_ERROR || (n) == M_UNDEFINED))

/* a68-parser-scope.cc  */

void a68_scope_checker (NODE_T *p);

/* a68-parser-serial-dsa.cc  */

void a68_serial_dsa (NODE_T *p);

/* a68-parser-pragmat.cc */

void a68_handle_pragmats (NODE_T *p);

/* a68-moids-diagnostics.cc  */

const char *a68_mode_error_text (NODE_T *n, MOID_T *p, MOID_T *q, int context, int deflex, int depth);
void a68_cannot_coerce (NODE_T *p, MOID_T *from, MOID_T *to, int context, int deflex, int att);
void a68_warn_for_voiding (NODE_T *p, SOID_T *x, SOID_T *y, int c);
void a68_semantic_pitfall (NODE_T *p, MOID_T *m, int c, int u);

/* a68-low-misc.cc  */

tree a68_lower_assertion (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_jump (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_parameter (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_parameter_list (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_parameter_pack (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_operator (NODE_T *p, LOW_CTX_T ctx);

/* a68-low-moids.cc  */

void a68_lower_moids (MOID_T *m);
void a68_set_type_moid (tree type, MOID_T *m);
tree a68_row_elements_pointer_type (tree type);
tree a68_row_elements_type (tree type);
tree a68_triplet_type (void);

/* a68-low-bits.cc */

tree a68_get_bits_skip_tree (MOID_T *m);
tree a68_bits_width (tree type);
tree a68_bits_maxbits (tree type);
tree a68_bits_bin (MOID_T *m, tree val);
tree a68_bits_abs (MOID_T *m, tree bits);
tree a68_bits_leng (tree type, tree bits);
tree a68_bits_shorten (tree type, tree bits);
tree a68_bits_not (tree bits);
tree a68_bits_and (tree bits1, tree bits2);
tree a68_bits_ior (tree bits1, tree bits2);
tree a68_bits_xor (tree bits1, tree bits2);
tree a68_bits_elem (NODE_T *p, tree pos, tree bits);
tree a68_bits_subset (tree bits1, tree bits2);
tree a68_bits_shift (tree shift, tree bits);
tree a68_bits_eq (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_bits_ne (tree a, tree b, location_t loc = UNKNOWN_LOCATION);

/* a68-low_bools.cc  */

tree a68_get_bool_skip_tree (void);
tree a68_bool_abs (tree val);
tree a68_bool_eq (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_bool_ne (tree a, tree b, location_t loc = UNKNOWN_LOCATION);

/* a68-low-ints.cc  */

tree a68_get_int_skip_tree (MOID_T *m);
tree a68_int_maxval (tree type);
tree a68_int_minval (tree type);
tree a68_int_width (tree type);
tree a68_int_sign (tree val);
tree a68_int_abs (tree val);
tree a68_int_shorten (MOID_T *to_mode, MOID_T *from_mode, tree val);
tree a68_int_leng (MOID_T *to_mode, MOID_T *from_mode, tree val);

tree a68_int_plus (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_minus (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_mult (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_div (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_mod (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_pow (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_eq (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_ne (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_lt (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_le (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_gt (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_int_ge (tree a, tree b, location_t loc = UNKNOWN_LOCATION);

/* a68-low-complex.cc  */

tree a68_complex_i (MOID_T *mode, tree re, tree im);
tree a68_complex_re (tree z);
tree a68_complex_im (tree z);
tree a68_complex_conj (MOID_T *mode, tree z);
tree a68_complex_widen_from_real (MOID_T *mode, tree r);

/* a68-low-posix.cc  */

tree a68_posix_setexitstatus (void);
tree a68_posix_argc (void);
tree a68_posix_argv (void);
tree a68_posix_getenv (void);
tree a68_posix_putchar (void);
tree a68_posix_puts (void);
tree a68_posix_fconnect (void);
tree a68_posix_fcreate (void);
tree a68_posix_fopen (void);
tree a68_posix_fclose (void);
tree a68_posix_fsize (void);
tree a68_posix_lseek (void);
tree a68_posix_errno (void);
tree a68_posix_perror (void);
tree a68_posix_strerror (void);
tree a68_posix_getchar (void);
tree a68_posix_fgetc (void);
tree a68_posix_fputc (void);
tree a68_posix_fputs (void);
tree a68_posix_gets (void);
tree a68_posix_fgets (void);

/* a68-low-reals.cc  */

tree a68_get_real_skip_tree (MOID_T *m);
tree a68_real_pi (tree type);
tree a68_real_maxval (tree type);
tree a68_real_minval (tree type);
tree a68_real_smallval (tree type);
tree a68_real_width (tree type);
tree a68_real_exp_width (tree type);
tree a68_real_sign (tree val);
tree a68_real_abs (tree val);
tree a68_real_sqrt (tree val);
tree a68_real_tan (tree type);
tree a68_real_sin (tree type);
tree a68_real_cos (tree type);
tree a68_real_acos (tree type);
tree a68_real_asin (tree type);
tree a68_real_atan (tree type);
tree a68_real_ln (tree type);
tree a68_real_log (tree type);
tree a68_real_exp (tree type);
tree a68_real_shorten (MOID_T *to_mode, MOID_T *from_mode, tree val);
tree a68_real_leng (MOID_T *to_mode, MOID_T *from_mode, tree val);
tree a68_real_entier (tree val, MOID_T *to_mode, MOID_T *from_mode);
tree a68_real_round (tree val, MOID_T *to_mode, MOID_T *from_mode);

tree a68_real_plus (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_minus (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_mult (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_div (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_mod (MOID_T *m, tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_pow (MOID_T *m, MOID_T *a_mode, MOID_T *b_mode,
		   tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_eq (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_ne (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_lt (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_le (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_gt (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_real_ge (tree a, tree b, location_t loc = UNKNOWN_LOCATION);


/* a68-low-strings.cc  */

tree a68_get_string_skip_tree (void);
tree a68_string_concat (tree str1, tree str2);
tree a68_string_mult (tree str1, tree str2);
tree a68_string_from_char (tree c);
tree a68_string_cmp (tree s1, tree s2);
char *a68_string_process_breaks (NODE_T *p, const char *str);

/* a68-low-chars.cc */

tree a68_get_char_skip_tree (void);
tree a68_char_max (void);
tree a68_char_repr (NODE_T *p, tree val);
tree a68_char_abs (tree val);
tree a68_char_eq (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_char_ne (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_char_lt (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_char_le (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_char_gt (tree a, tree b, location_t loc = UNKNOWN_LOCATION);
tree a68_char_ge (tree a, tree b, location_t loc = UNKNOWN_LOCATION);

/* a68-low-refs.cc  */

tree a68_get_ref_skip_tree (MOID_T *m);

/* a68-low-procs.cc  */

tree a68_get_proc_skip_tree (MOID_T *m);

/* a68-low-structs.cc  */

tree a68_get_struct_skip_tree (MOID_T *m);

/* a68-low-multiples.cc  */

tree a68_get_multiple_skip_tree (MOID_T *m);
tree a68_multiple_dimensions (tree exp);
tree a68_multiple_num_elems (tree exp);
tree a68_multiple_lower_bound (tree exp, tree dim);
tree a68_multiple_upper_bound (tree exp, tree dim);
tree a68_multiple_stride (tree exp, tree dim);
tree a68_multiple_triplets (tree exp);
tree a68_multiple_elements (tree exp);
tree a68_multiple_elements_size (tree exp);
tree a68_multiple_set_elements (tree exp, tree elements);
tree a68_multiple_set_elements_size (tree exp, tree elements_size);
void a68_multiple_compute_strides (tree type, size_t dim,
				   tree *lower_bounds, tree *upper_bounds,
				   tree *strides);
tree a68_multiple_set_lower_bound (tree exp, tree dim, tree bound);
tree a68_multiple_set_upper_bound (tree exp, tree dim, tree bound);
tree a68_multiple_set_stride (tree exp, tree dim, tree stride);
tree a68_row_value (tree type, size_t dim,
		    tree elements, tree elements_size,
		    tree *lower_bound, tree *upper_bound);
tree a68_row_value_raw (tree type, tree descriptor,
			tree elements, tree elements_size);
tree a68_row_malloc (tree type, int dim,
		    tree elements, tree elements_size,
		    tree *lower_bound, tree *upper_bound);		     
tree a68_multiple_slice (NODE_T *p, tree multiple, bool slicing_name,
			 int num_indexes, tree *indexes);
tree a68_multiple_copy_elems (MOID_T *to_mode, tree to, tree from);
tree a68_rows_dim (tree exp);
tree a68_rows_value (tree multiple);
tree a68_rows_lower_bound (tree rows, tree dim);
tree a68_rows_upper_bound (tree rows, tree dim);
tree a68_rows_dim_check (NODE_T *p, tree rows, tree dim);
tree a68_multiple_dim_check (NODE_T *p, tree multiple, tree dim);
tree a68_multiple_single_bound_check (NODE_T *p, tree dim, tree multiple,
				      tree index, bool upper_bound);
tree a68_multiple_bounds_check (NODE_T *p, tree dim, tree multiple,
				tree index);
tree a68_multiple_bounds_check_equal (NODE_T *p, tree m1, tree m2);

/* a68-low-ranges.cc  */

bool a68_in_global_range (void);
void a68_init_ranges (void);
void a68_push_range (MOID_T *mode);
tree a68_pop_range (void);
tree a68_pop_range_with_finalizer (tree *finalizer);
void a68_push_stmt_list (MOID_T *mode);
tree a68_pop_stmt_list (void);
void a68_push_function_range (tree fndel, tree result_type,
			      bool top_level = false);
void a68_pop_function_range (tree body);
void a68_push_serial_clause_range (MOID_T *clause_mode,
				   bool save_restore_stack = false);
tree a68_pop_serial_clause_range (void);
void a68_add_stmt (tree exp);
void a68_add_decl (tree decl);
void a68_add_decl_expr (tree decl_expr);
void a68_add_completer (void);
tree a68_range_context (void);
tree a68_range_names (void);
tree a68_range_stmt_list (void);

/* a68-low-runtime.cc */

enum a68_libcall_fn
{
#define DEF_A68_RUNTIME(CODE, N, T, P, F) A68_LIBCALL_ ## CODE,
#include "a68-low-runtime.def"
#undef DEF_A68_RUNTIME
  A68_LIBCALL_LAST
};

tree a68_get_libcall (a68_libcall_fn libcall);
tree a68_build_libcall (a68_libcall_fn libcall, tree type, int nargs, ...);

/* a68-low-clauses.cc  */

void a68_begin_serial_clause (LOW_CTX_T *ctx, MOID_T *clause_mode);
tree a68_finish_serial_clause (LOW_CTX_T ctx, MOID_T *clause_mode, tree parent_block);
tree a68_lower_label (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_labeled_unit (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_completer (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_initialiser_series (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_serial_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_loop_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_conformity_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_case_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_enquiry_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_conditional_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_unit_list (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_access_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_collateral_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_parallel_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_closed_clause (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_enclosed_clause (NODE_T *p, LOW_CTX_T ctx);

/* a68-low-coercions.cc */

tree a68_lower_dereferencing (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_rowing (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_widening (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_deproceduring (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_proceduring (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_voiding (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_uniting (NODE_T *p, LOW_CTX_T ctx);

/* a68-low-decls.cc */

tree a68_lower_mode_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_variable_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_identity_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_procedure_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_procedure_variable_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_declarer (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_declaration_list (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_priority_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_brief_operator_declaration (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_operator_declaration (NODE_T *p, LOW_CTX_T ctx);

/* a68-low.cc  */

tree a68_lower_top_tree (NODE_T *p);
tree a68_lower_tree (NODE_T *p, LOW_CTX_T ctx);
tree a68_make_identity_declaration_decl (NODE_T *identifier, const char *module_name = NULL,
					 bool indicant = false, bool external = false,
					 const char *extern_symbol = NULL);
tree a68_make_variable_declaration_decl (NODE_T *identifier, const char *module_name = NULL,
					 bool external = false,
					 const char *extern_symbol = NULL);
tree a68_make_proc_identity_declaration_decl (NODE_T *identifier, const char *module_name = NULL,
					      bool indicant = false, bool external = false,
					      const char *extern_symbol = NULL);
tree a68_make_anonymous_routine_decl (MOID_T *mode);
tree a68_get_skip_tree (MOID_T *m);
tree a68_get_empty (void);
void a68_ref_counts (tree exp, MOID_T *m, int *num_refs, int *num_pointers);
tree a68_consolidate_ref (MOID_T *m, tree expr);
tree a68_lower_alloca (tree type, tree size);
tree a68_lower_malloc (tree type, tree size);
tree a68_checked_indirect_ref (NODE_T *p, tree exp, MOID_T *exp_mode);
tree a68_low_deref (tree exp, NODE_T *p);
tree a68_low_dup (tree exp, bool use_heap = false);
tree a68_low_ascription (MOID_T *mode, tree lhs, tree rhs);
tree a68_low_assignation (NODE_T *p, tree lhs, MOID_T *lhs_mode, tree rhs, MOID_T *rhs_mode);
tree a68_lower_memcpy (tree dst, tree src, tree size);
tree a68_lower_tmpvar (const char *name, tree type, tree init);
tree a68_get_mangled_identifier (const char *name,
				 const char *mname = NULL, bool internal = false,
				 bool numbered = false);
tree a68_get_mangled_indicant (const char *name,
			       const char *mname = NULL, bool internal = false,
			       bool numbered = false);
char *a68_demangle_symbol (const char *mname, const char *symbol,
			   bool is_operator = false);
tree a68_low_toplevel_func_decl (const char *name, tree fntype);
tree a68_low_func_param (tree fndecl, const char *name, tree type);

/* a68-low-builtins.cc */

void a68_install_builtins ();

/* a68-low-unions.c  */

int a68_united_mode_index (MOID_T *p, MOID_T *q);
tree a68_get_union_skip_tree (MOID_T *m);
tree a68_union_overhead (tree exp);
tree a68_union_set_overhead (tree exp, tree overhead);
tree a68_union_cunion (tree exp);
tree a68_union_alternative (tree exp, int index);
tree a68_union_value (MOID_T *mode, tree exp, MOID_T *exp_mode);
tree a68_union_translate_overhead (MOID_T *from, tree from_overhead, MOID_T *to);
bool a68_union_contains_mode (MOID_T *p, MOID_T *q);

/* a68-low-units.cc  */

tree a68_lower_identifier (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_denotation (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_denotation (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_skip (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_nihil (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_empty (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_identity_relation (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_logic_function (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_primary (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_cast (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_secondary (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_slice (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_selection (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_formula (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_monadic_formula (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_tertiary (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_assignation (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_routine_text (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_generator (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_call (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_unit (NODE_T *p, LOW_CTX_T ctx);

/* a68-low-generator.c  */

tree a68_low_generator (NODE_T *declarer, MOID_T *mode,
			bool heap, LOW_CTX_T ctx);
tree a68_low_gen (MOID_T *m, size_t nbuonds, tree *bounds,
		  bool use_heap);

/* a68-low-prelude.c  */

tree a68_lower_unimplemented (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_assert (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_intabs2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_realabs2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_boolabs2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_charabs2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_not2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_and3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_or3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_xor3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_confirm2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_negate2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_sign2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_realsign2 (NODE_T *p, LOW_CTX_T ctx);

tree a68_lower_plus_int (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_plus_real (NODE_T *p, LOW_CTX_T ctx);

tree a68_lower_minus_int (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_minus_real (NODE_T *p, LOW_CTX_T ctx);

tree a68_lower_mult_int (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_mult_real (NODE_T *p, LOW_CTX_T ctx);

tree a68_lower_multab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_div3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_divab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_rdiv3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_over3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_mod3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_int_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_int_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_int_lt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_int_le3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_int_gt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_int_ge3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_real_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_real_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_real_lt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_real_le3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_real_gt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_real_ge3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_lt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_le3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_gt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_ge3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bool_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bool_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_plusab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_minusab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_overab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_modab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_upb2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_upb3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_lwb2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_lwb3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_elems2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_elems3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_entier2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_round2 (NODE_T *p, LOW_CTX_T ctx);

tree a68_lower_pow_int (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_pow_real (NODE_T *p, LOW_CTX_T ctx);

tree a68_lower_odd2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_lt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_le3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_gt3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_ge3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_plus3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_plus3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_plusab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_mult3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_char_mult3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_multab3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_string_plusto3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_repr2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitelem3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bin2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitabs2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitleng2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitshorten2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bit_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bit_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitnot2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitand3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitior3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitxor3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shl3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shr3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bit_eq3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bit_ne3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bit_le3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bit_ge3 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_maxint (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_minint (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_maxbits (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_maxreal (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_minreal (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_smallreal (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitswidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longbitswidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlongbitswidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shortbitswidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shortshortbitswidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_intwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longintwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlongintwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shortintwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shortshortintwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_realwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longrealwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlongrealwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_expwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longexpwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlongexpwidth (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_pi (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_nullcharacter (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_flip (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_flop (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_errorchar (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_blank (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_eofchar (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_replacementchar (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_intlengths (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_intshorths (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitslengths (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_bitsshorths (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_reallengths (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_realshorths (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_infinity (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_minusinfinity (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_maxabschar (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_sqrt (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_sqrt (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_sqrt (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_tan (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_tan (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_tan (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_sin (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_sin (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_sin (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_cos (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_cos (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_cos (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_acos (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_acos (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_acos (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_asin (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_asin (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_asin (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_atan (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_atan (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_atan (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_ln (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_ln (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_ln (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_log (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_log (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_log (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_exp (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_exp (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_long_long_exp (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_reali (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longreali (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlongreali (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_inti (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longinti (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlonginti (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_re2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_im2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_conj2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shortenint2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_lengint2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_lengreal2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_shortenreal2 (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_random (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longrandom (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_longlongrandom (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_setexitstatus (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixargc (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixargv (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixputchar (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixputs (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfputc (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfputs (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixgetenv (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfconnect (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfopen (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfcreate (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfclose (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfsize (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixlseek (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixseekcur (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixseekend (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixseekset (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixstdinfiledes (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixstdoutfiledes (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixstderrfiledes (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfileodefault (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfileordwr (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfileordonly (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfileowronly (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfileotrunc (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixerrno (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixperror (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixstrerror (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixgetchar (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfgetc (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixgets (NODE_T *p, LOW_CTX_T ctx);
tree a68_lower_posixfgets (NODE_T *p, LOW_CTX_T ctx);

/* a68-exports.cc  */

MOIF_T *a68_moif_new (const char *module_name);
void a68_moif_free (MOIF_T *moif);
void a68_do_exports (NODE_T *p);

/* a68-imports.cc  */

MOIF_T *a68_open_packet (const char *module);
bool a68_process_module_map (const char *map, const char **errmsg);

/* a68-parser-debug.cc  */

void a68_dump_parse_tree (NODE_T *p, bool tables = false, bool levels = false);
void a68_dump_modes (MOID_T *m);
void a68_dump_moif (MOIF_T *moif);

#endif /* ! __A68_H__ */
