/* Implement I/O-related actions for CHILL.
   Copyright (C) 1992, 93, 94, 98, 99, 2000 Free Software Foundation, Inc.
   
   This file is part of GNU CC.
   
   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "ch-tree.h"
#include "rtl.h"
#include "lex.h"
#include "flags.h"
#include "input.h"
#include "assert.h"
#include "toplev.h"

/* set non-zero if input text is forced to lowercase */
extern int ignore_case;

/* set non-zero if special words are to be entered in uppercase */
extern int special_UC;

static int intsize_of_charsexpr		PARAMS ((tree));
static tree add_enum_to_list		PARAMS ((tree, tree));
static void build_chill_io_list_type	PARAMS ((void));
static void build_io_types		PARAMS ((void));
static void declare_predefined_file	PARAMS ((const char *, const char *));
static tree build_access_part	        PARAMS ((void));
static tree textlocation_mode		PARAMS ((tree));
static int check_assoc			PARAMS ((tree, int, const char *));
static tree assoc_call			PARAMS ((tree, tree, const char *));
static int check_transfer		PARAMS ((tree, int, const char *));
static int connect_process_optionals	PARAMS ((tree, tree *, tree *, tree));
static tree connect_text		PARAMS ((tree, tree, tree, tree));
static tree connect_access		PARAMS ((tree, tree, tree, tree));
static int check_access			PARAMS ((tree, int, const char *));
static int check_text			PARAMS ((tree, int, const char *));
static tree get_final_type_and_range	PARAMS ((tree, tree *, tree *));
static void process_io_list		PARAMS ((tree, tree *, tree *, rtx *,
						int, int));
static void check_format_string		PARAMS ((tree, tree, int));
static int get_max_size			PARAMS ((tree));

/* association mode */
tree association_type_node;
/* initialzier for association mode */
tree association_init_value;

/* NOTE: should be same as in runtime/chillrt0.c */
#define STDIO_TEXT_LENGTH    1024
/* mode of stdout, stdin, stderr*/
static tree stdio_type_node;

/* usage- and where modes */
tree usage_type_node;
tree where_type_node;

/* we have to distinguish between io-list-type for WRITETEXT
   and for READTEXT. WRITETEXT does not process ranges and
   READTEXT must get pointers to the variables.
   */
/* variable to hold the type of the io_list */
static tree chill_io_list_type = NULL_TREE;

/* the type for the enum tables */
static tree enum_table_type = NULL_TREE;

/* structure to save enums for later use in compilation */
typedef struct save_enum_names
{
  struct save_enum_names  *forward;
  tree			  name;
  tree			  decl;
} SAVE_ENUM_NAMES;

static SAVE_ENUM_NAMES *used_enum_names = (SAVE_ENUM_NAMES *)0;

typedef struct save_enum_values
{
  long		 	  val;
  struct save_enum_names  *name;
} SAVE_ENUM_VALUES;

typedef struct save_enums
{
  struct save_enums       *forward;
  tree		          context;
  tree		          type;
  tree		          ptrdecl;
  long		          num_vals;
  struct save_enum_values *vals;
} SAVE_ENUMS;

static SAVE_ENUMS	*used_enums = (SAVE_ENUMS *)0;


/* Function collects all enums are necessary to collect, makes a copy of
   the value and returns a VAR_DECL external to current function describing
   the pointer to a name table, which will be generated at the end of
   compilation
   */

static tree add_enum_to_list (type, context)
     tree  type;
     tree  context;
{
  tree		tmp;
  SAVE_ENUMS		*wrk = used_enums;
  SAVE_ENUM_VALUES	*vals;
  SAVE_ENUM_NAMES	*names;
    
  while (wrk != (SAVE_ENUMS *)0)
    {
      /* search for this enum already in use */
      if (wrk->context == context && wrk->type == type)
	{
	  /* yes, found. look if the ptrdecl is valid in this scope */
	  char  *name = IDENTIFIER_POINTER (DECL_NAME (wrk->ptrdecl));
	  tree   var  = get_identifier (name);
	  tree   decl = lookup_name (var);
	    
	  if (decl == NULL_TREE)
	    {
	      /* no, not valid in this context, declare it */
	      decl = decl_temp1 (var, build_pointer_type (TREE_TYPE (enum_table_type)),
				 0, NULL_TREE, 1, 0);
	    }
	  return decl;
	}
	
      /* next one */
      wrk = wrk->forward;
    }
    
  /* not yet found -- generate an entry */
  wrk = (SAVE_ENUMS *)xmalloc (sizeof (SAVE_ENUMS));
  wrk->forward = used_enums;
  used_enums = wrk;
    
  /* generate the pointer decl */
  wrk->ptrdecl = get_unique_identifier ("ENUMTABPTR");
  wrk->ptrdecl = decl_temp1 (wrk->ptrdecl, build_pointer_type (TREE_TYPE (enum_table_type)),
			     0, NULL_TREE, 1, 0);

  /* save information for later use */
  wrk->context = context;
  wrk->type = type;

  /* insert the names and values */
  tmp = TYPE_FIELDS (type);
  wrk->num_vals = list_length (tmp);
  vals = (SAVE_ENUM_VALUES *)xmalloc (sizeof (SAVE_ENUM_VALUES) * wrk->num_vals);
  wrk->vals = vals;
    
  while (tmp != NULL_TREE)
    {
      /* search if name is already in use */
      names = used_enum_names;
      while (names != (SAVE_ENUM_NAMES *)0)
	{
	  if (names->name == TREE_PURPOSE (tmp))
	    break;
	  names = names->forward;
	}
      if (names == (SAVE_ENUM_NAMES *)0)
	{
	  /* we have to insert one */
	  names = (SAVE_ENUM_NAMES *)xmalloc (sizeof (SAVE_ENUM_NAMES));
	  names->forward = used_enum_names;
	  used_enum_names = names;
	  names->decl = NULL_TREE;
	  names->name = TREE_PURPOSE (tmp);
	}
      vals->name = names;
      vals->val = TREE_INT_CST_LOW (TREE_VALUE (tmp));
	
      /* next entry in enum */
      vals++;
      tmp = TREE_CHAIN (tmp);
    }
    
  /* return the generated decl */
  return wrk->ptrdecl;
}


static void
build_chill_io_list_type ()
{
  tree list = NULL_TREE;
  tree result, enum1, listbase;
  tree io_descriptor;
  tree decl1, decl2;
  tree forcharstring, forset_W, forset_R, forboolrange;

  tree forintrange, intunion, forsetrange, forcharrange;
  tree long_type, ulong_type, union_type;
    
  long_type = long_integer_type_node;
  ulong_type = long_unsigned_type_node;

  if (chill_io_list_type != NULL_TREE)
    /* already done */
    return;

  /* first build the enum for the desriptor */
  enum1 = start_enum (NULL_TREE);
  result = build_enumerator (get_identifier ("__IO_UNUSED"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_ByteVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_UByteVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_IntVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_UIntVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_LongVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_ULongVal"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_ByteLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_UByteLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_IntLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_UIntLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_LongLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_ULongLoc"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_ByteRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_UByteRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_IntRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_UIntRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_LongRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_ULongRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_BoolVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_BoolLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_BoolRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_SetVal"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_SetLoc"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_SetRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_CharVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_CharLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_CharRangeLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_CharStrLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_CharVaryingLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_BitStrLoc"),
			     NULL_TREE);
  list = chainon (result, list);

  result = build_enumerator (get_identifier ("__IO_RealVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_RealLoc"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_LongRealVal"),
			     NULL_TREE);
  list = chainon (result, list);
    
  result = build_enumerator (get_identifier ("__IO_LongRealLoc"),
			     NULL_TREE);
  list = chainon (result, list);
#if 0    
  result = build_enumerator (get_identifier ("_IO_Pointer"),
			     NULL_TREE);
  list = chainon (result, list);
#endif    

  result = finish_enum (enum1, list);
  pushdecl (io_descriptor = build_decl (TYPE_DECL,
					get_identifier ("__tmp_IO_enum"),
					result));
  /* prevent seizing/granting of the decl */
  DECL_SOURCE_LINE (io_descriptor) = 0;
  satisfy_decl (io_descriptor, 0);

  /* build type for enum_tables */
  decl1 = build_decl (FIELD_DECL, get_identifier ("value"),
		      long_type);
  DECL_INITIAL (decl1) = NULL_TREE;
  decl2 = build_decl (FIELD_DECL, get_identifier ("name"),
		      build_pointer_type (char_type_node));
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
  result = build_chill_struct_type (decl1);
  pushdecl (enum_table_type = build_decl (TYPE_DECL,
					  get_identifier ("__tmp_IO_enum_table_type"),
					  result));
  DECL_SOURCE_LINE (enum_table_type) = 0;
  satisfy_decl (enum_table_type, 0);

  /* build type for writing a set mode */
  decl1 = build_decl (FIELD_DECL, get_identifier ("value"),
		      long_type);
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("name_table"),
		      build_pointer_type (TREE_TYPE (enum_table_type)));
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
    
  result = build_chill_struct_type (listbase);
  pushdecl (forset_W = build_decl (TYPE_DECL,
				   get_identifier ("__tmp_WIO_set"),
				   result));
  DECL_SOURCE_LINE (forset_W) = 0;
  satisfy_decl (forset_W, 0);

  /* build type for charrange */
  decl1 = build_decl (FIELD_DECL, get_identifier ("ptr"),
		      build_pointer_type (char_type_node));
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("lower"),
		      long_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("upper"),
		      long_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
    
  result = build_chill_struct_type (listbase);
  pushdecl (forcharrange = build_decl (TYPE_DECL,
				       get_identifier ("__tmp_IO_charrange"),
				       result));
  DECL_SOURCE_LINE (forcharrange) = 0;
  satisfy_decl (forcharrange, 0);
    
  /* type for integer range */
  decl1 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("_slong"),
				       long_type));
  listbase = decl1;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("_ulong"),
				       ulong_type));
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;

  decl1 = grok_chill_variantdefs (NULL_TREE, listbase, NULL_TREE);
  TREE_CHAIN (decl1) = NULL_TREE;
  result = build_chill_struct_type (decl1);
  pushdecl (intunion = build_decl (TYPE_DECL,
				   get_identifier ("__tmp_IO_long"),
				   result));
  DECL_SOURCE_LINE (intunion) = 0;
  satisfy_decl (intunion, 0);

  decl1 = build_decl (FIELD_DECL,
		      get_identifier ("ptr"),
		      ptr_type_node);
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("lower"),
		      TREE_TYPE (intunion));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("upper"),
		      TREE_TYPE (intunion));
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;

  result = build_chill_struct_type (listbase);
  pushdecl (forintrange = build_decl (TYPE_DECL,
				      get_identifier ("__tmp_IO_intrange"),
				      result));
  DECL_SOURCE_LINE (forintrange) = 0;
  satisfy_decl (forintrange, 0);

  /* build structure for bool range */
  decl1 = build_decl (FIELD_DECL,
		      get_identifier ("ptr"),
		      ptr_type_node);
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("lower"),
		      ulong_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("upper"),
		      ulong_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;

  result = build_chill_struct_type (listbase);
  pushdecl (forboolrange = build_decl (TYPE_DECL,
				       get_identifier ("__tmp_RIO_boolrange"),
				       result));
  DECL_SOURCE_LINE (forboolrange) = 0;
  satisfy_decl (forboolrange, 0);

  /* build type for reading a set */
  decl1 = build_decl (FIELD_DECL, get_identifier ("ptr"),
		      ptr_type_node);
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("length"),
		      long_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL, get_identifier ("name_table"),
		      build_pointer_type (TREE_TYPE (enum_table_type)));
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
    
  result = build_chill_struct_type (listbase);
  pushdecl (forset_R = build_decl (TYPE_DECL,
				   get_identifier ("__tmp_RIO_set"),
				   result));
  DECL_SOURCE_LINE (forset_R) = 0;
  satisfy_decl (forset_R, 0);
    
  /* build type for setrange */
  decl1 = build_decl (FIELD_DECL, get_identifier ("ptr"),
		      ptr_type_node);
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("length"),
		      long_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("name_table"),
		      build_pointer_type (TREE_TYPE (enum_table_type)));
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("lower"),
		      long_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_decl (FIELD_DECL, get_identifier ("upper"),
		      long_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
    
  result = build_chill_struct_type (listbase);
  pushdecl (forsetrange = build_decl (TYPE_DECL,
				      get_identifier ("__tmp_RIO_setrange"),
				      result));
  DECL_SOURCE_LINE (forsetrange) = 0;
  satisfy_decl (forsetrange, 0);

  /* build structure for character string */
  decl1 = build_decl (FIELD_DECL, 
		      get_identifier ("string"),
		      build_pointer_type (char_type_node));
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;
    
  decl2 = build_decl (FIELD_DECL, 
		      get_identifier ("string_length"),
		      ulong_type);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
    
  result = build_chill_struct_type (listbase);
  pushdecl (forcharstring = build_decl (TYPE_DECL,
					get_identifier ("__tmp_IO_forcharstring"), result));
  DECL_SOURCE_LINE (forcharstring) = 0;
  satisfy_decl (forcharstring, 0);

  /* build the union */
  decl1 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valbyte"),
				       signed_char_type_node));
  listbase = decl1;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valubyte"),
				       unsigned_char_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valint"),
				       chill_integer_type_node)); 
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valuint"),
				       chill_unsigned_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__vallong"),
				       long_type));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valulong"),
				       ulong_type));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locint"),
				       ptr_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locintrange"),
				       TREE_TYPE (forintrange)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valbool"),
				       boolean_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locbool"),
				       build_pointer_type (boolean_type_node)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locboolrange"),
				       TREE_TYPE (forboolrange)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valset"),
				       TREE_TYPE (forset_W)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locset"),
				       TREE_TYPE (forset_R)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locsetrange"),
				       TREE_TYPE (forsetrange)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valchar"),
				       char_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locchar"),
				       build_pointer_type (char_type_node)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__loccharrange"),
				       TREE_TYPE (forcharrange)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__loccharstring"),
				       TREE_TYPE (forcharstring)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__valreal"),
				       float_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__locreal"),
				       build_pointer_type (float_type_node)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__vallongreal"),
				       double_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__loclongreal"),
				       build_pointer_type (double_type_node)));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

#if 0    
  decl2 = build_tree_list (NULL_TREE,
			   build_decl (FIELD_DECL,
				       get_identifier ("__forpointer"),
				       ptr_type_node));
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
#endif

  TREE_CHAIN (decl2) = NULL_TREE;
    
  decl1 = grok_chill_variantdefs (NULL_TREE, listbase, NULL_TREE);
  TREE_CHAIN (decl1) = NULL_TREE;
  result = build_chill_struct_type (decl1);
  pushdecl (union_type = build_decl (TYPE_DECL,
				     get_identifier ("__tmp_WIO_union"),
				     result));
  DECL_SOURCE_LINE (union_type) = 0;
  satisfy_decl (union_type, 0);
    
  /* now build the final structure */
  decl1 = build_decl (FIELD_DECL, get_identifier ("__t"),
		      TREE_TYPE (union_type));
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL, get_identifier ("__descr"),
		      long_type);
    
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
    
  result = build_chill_struct_type (listbase);
  pushdecl (chill_io_list_type = build_decl (TYPE_DECL,
					     get_identifier ("__tmp_IO_list"),
					     result));
  DECL_SOURCE_LINE (chill_io_list_type) = 0;
  satisfy_decl (chill_io_list_type, 0);
}

/* build the ASSOCIATION, ACCESS and TEXT mode types */
static void
build_io_types ()
{
  tree listbase, decl1, decl2, result, association;
  tree acc, txt, tloc;
  tree enum1, tmp;

  /* the association mode */
  listbase = build_decl (FIELD_DECL,
			 get_identifier ("flags"),
			 long_unsigned_type_node);
  DECL_INITIAL (listbase) = NULL_TREE;
  decl1 = listbase;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("pathname"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("access"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("handle"),
		      integer_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("bufptr"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("syserrno"),
		      long_integer_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("usage"),
		      char_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("ctl_pre"),
		      char_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("ctl_post"),
		      char_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;

  result = build_chill_struct_type (listbase);
  pushdecl (association = build_decl (TYPE_DECL,
				      ridpointers[(int)RID_ASSOCIATION],
				      result));
  DECL_SOURCE_LINE (association) = 0;
  satisfy_decl (association, 0);
  association_type_node = TREE_TYPE (association);
  TYPE_NAME (association_type_node) = association;
  CH_NOVELTY (association_type_node) = association;
  CH_TYPE_NONVALUE_P(association_type_node) = 1;
  CH_TYPE_NONVALUE_P(association) = 1;

  /* initialiser for association type */
  tmp = convert (char_type_node, integer_zero_node);
  association_init_value =
    build_nt (CONSTRUCTOR, NULL_TREE,
      tree_cons (NULL_TREE, integer_zero_node,            /* flags */
        tree_cons (NULL_TREE, null_pointer_node,          /* pathname */
          tree_cons (NULL_TREE, null_pointer_node,        /* access */
            tree_cons (NULL_TREE, integer_minus_one_node, /* handle */
              tree_cons (NULL_TREE, null_pointer_node,    /* bufptr */
                tree_cons (NULL_TREE, integer_zero_node,  /* syserrno */
                  tree_cons (NULL_TREE, tmp,              /* usage */
                    tree_cons (NULL_TREE, tmp,            /* ctl_pre */
                      tree_cons (NULL_TREE, tmp,          /* ctl_post */
				 NULL_TREE))))))))));

  /* the type for stdin, stdout, stderr */
  /* text part */
  decl1 = build_decl (FIELD_DECL,
		      get_identifier ("flags"),
		      long_unsigned_type_node);
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("text_record"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("access_sub"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("actual_index"),
		      long_unsigned_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
  txt = build_chill_struct_type (listbase);

  /* access part */
  decl1 = build_decl (FIELD_DECL,
		      get_identifier ("flags"),
		      long_unsigned_type_node);
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("reclength"),
		      long_unsigned_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;
  
  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("lowindex"),
		      long_integer_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("highindex"),
		      long_integer_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl2 = decl1;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("association"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("base"),
		      long_unsigned_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("storelocptr"),
		      ptr_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL,
		      get_identifier ("rectype"),
		      long_integer_type_node);
  DECL_INITIAL (decl2) = NULL_TREE;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;
  acc = build_chill_struct_type (listbase);

  /* the location */
  tmp = build_string_type (char_type_node, build_int_2 (STDIO_TEXT_LENGTH, 0));
  tloc = build_varying_struct (tmp);

  /* now the final mode */
  decl1 = build_decl (FIELD_DECL, get_identifier ("txt"), txt);
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL, get_identifier ("acc"), acc);
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL, get_identifier ("tloc"), tloc);
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_lang_decl (TYPE_DECL, get_identifier ("__indexmode"),
			   void_type_node);
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (CONST_DECL, get_identifier ("__textlength"),
		      integer_type_node);
  DECL_INITIAL (decl2) = build_int_2 (STDIO_TEXT_LENGTH, 0);
  TREE_CHAIN (decl1) = decl2;
  decl1 = decl2;

  decl2 = build_decl (CONST_DECL, get_identifier ("__dynamic"),
		      integer_type_node);
  DECL_INITIAL (decl2) = integer_zero_node;
  TREE_CHAIN (decl1) = decl2;
  TREE_CHAIN (decl2) = NULL_TREE;

  result = build_chill_struct_type (listbase);
  pushdecl (tmp = build_decl (TYPE_DECL,
			      get_identifier ("__stdio_text"),
			      result));
  DECL_SOURCE_LINE (tmp) = 0;
  satisfy_decl (tmp, 0);
  stdio_type_node = TREE_TYPE (tmp);
  CH_IS_TEXT_MODE (stdio_type_node) = 1;

  /* predefined usage mode */
  enum1 = start_enum (NULL_TREE);
  listbase = NULL_TREE;
  result = build_enumerator (
            get_identifier ((ignore_case || ! special_UC) ? "readonly" : "READONLY"),
			     NULL_TREE);
  listbase = chainon (result, listbase);
  result = build_enumerator (
            get_identifier ((ignore_case || ! special_UC) ? "writeonly" : "WRITEONLY"),
			     NULL_TREE);
  listbase = chainon (result, listbase);
  result = build_enumerator (
            get_identifier ((ignore_case || ! special_UC) ? "readwrite" : "READWRITE"),
			     NULL_TREE);
  listbase = chainon (result, listbase);
  result = finish_enum (enum1, listbase);
  pushdecl (tmp = build_decl (TYPE_DECL,
			      get_identifier ((ignore_case || ! special_UC) ? "usage" : "USAGE"),
			      result));
  DECL_SOURCE_LINE (tmp) = 0;
  satisfy_decl (tmp, 0);
  usage_type_node = TREE_TYPE (tmp);
  TYPE_NAME (usage_type_node) = tmp;
  CH_NOVELTY (usage_type_node) = tmp;

  /* predefined where mode */
  enum1 = start_enum (NULL_TREE);
  listbase = NULL_TREE;
  result = build_enumerator (
            get_identifier ((ignore_case || ! special_UC) ? "first" : "FIRST"),
			     NULL_TREE);
  listbase = chainon (result, listbase);
  result = build_enumerator (
            get_identifier ((ignore_case || ! special_UC) ? "same" : "SAME"),
			     NULL_TREE);
  listbase = chainon (result, listbase);
  result = build_enumerator (
            get_identifier ((ignore_case || ! special_UC) ? "last" : "LAST"),
			     NULL_TREE);
  listbase = chainon (result, listbase);
  result = finish_enum (enum1, listbase);
  pushdecl (tmp = build_decl (TYPE_DECL,
			      get_identifier ((ignore_case || ! special_UC) ? "where" : "WHERE"),
			      result));
  DECL_SOURCE_LINE (tmp) = 0;
  satisfy_decl (tmp, 0);
  where_type_node = TREE_TYPE (tmp);
  TYPE_NAME (where_type_node) = tmp;
  CH_NOVELTY (where_type_node) = tmp;
}

static void
declare_predefined_file (name, assembler_name)
     const char *name;
     const char *assembler_name;
{
  tree decl = build_lang_decl (VAR_DECL, get_identifier (name),
			       stdio_type_node);
  DECL_ASSEMBLER_NAME (decl) = get_identifier(assembler_name);
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  DECL_IN_SYSTEM_HEADER (decl) = 1;
  make_decl_rtl (decl, 0, 1);
  pushdecl (decl);
}


/* initialisation of all IO/related functions, types, etc. */
void
inout_init ()
{
  /* We temporarily reset the maximum_field_alignment to zero so the
     compiler's init data structures can be compatible with the
     run-time system, even when we're compiling with -fpack. */
  unsigned int save_maximum_field_alignment = maximum_field_alignment;

  extern tree chill_predefined_function_type;
  tree endlink = void_list_node;
  tree bool_ftype_ptr_ptr_int;
  tree ptr_ftype_ptr_ptr_int;
  tree luns_ftype_ptr_ptr_int;
  tree int_ftype_ptr_ptr_int;
  tree ptr_ftype_ptr_ptr_int_ptr_int_ptr_int;
  tree void_ftype_ptr_ptr_int_ptr_int_ptr_int;
  tree void_ftype_ptr_ptr_int;
  tree void_ftype_ptr_ptr_int_int_int_long_ptr_int;
  tree ptr_ftype_ptr_int_ptr_ptr_int;
  tree void_ftype_ptr_int_ptr_luns_ptr_int;
  tree void_ftype_ptr_ptr_ptr_int;
  tree void_ftype_ptr_int_ptr_int;
  tree void_ftype_ptr_int_ptr_int_ptr_int_ptr_int;

  maximum_field_alignment = 0;

  builtin_function ((ignore_case || ! special_UC) ? "associate" : "ASSOCIATE",
		    chill_predefined_function_type,
		    BUILT_IN_ASSOCIATE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "connect" : "CONNECT",
		    chill_predefined_function_type,
		    BUILT_IN_CONNECT, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "create" : "CREATE",
		    chill_predefined_function_type,
		    BUILT_IN_CREATE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "delete" : "DELETE",
		    chill_predefined_function_type,
		    BUILT_IN_CH_DELETE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "disconnect" : "DISCONNECT",
		    chill_predefined_function_type,
		    BUILT_IN_DISCONNECT, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "dissociate" : "DISSOCIATE",
		    chill_predefined_function_type,
		    BUILT_IN_DISSOCIATE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "eoln" : "EOLN",
		    chill_predefined_function_type,
		    BUILT_IN_EOLN, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "existing" : "EXISTING",
		    chill_predefined_function_type,
		    BUILT_IN_EXISTING, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "getassociation" : "GETASSOCIATION",
		    chill_predefined_function_type,
		    BUILT_IN_GETASSOCIATION, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "gettextaccess" : "GETTEXTASSCESS",
		    chill_predefined_function_type,
		    BUILT_IN_GETTEXTACCESS, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "gettextindex" : "GETTEXTINDEX",
		    chill_predefined_function_type,
		    BUILT_IN_GETTEXTINDEX, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "gettextrecord" : "GETTEXTRECORD",
		    chill_predefined_function_type,
		    BUILT_IN_GETTEXTRECORD, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "getusage" : "GETUSAGE",
		    chill_predefined_function_type,
		    BUILT_IN_GETUSAGE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "indexable" : "INDEXABLE",
		    chill_predefined_function_type,
		    BUILT_IN_INDEXABLE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "isassociated" : "ISASSOCIATED",
		    chill_predefined_function_type,
		    BUILT_IN_ISASSOCIATED, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "modify" : "MODIFY",
		    chill_predefined_function_type,
		    BUILT_IN_MODIFY, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "outoffile" : "OUTOFFILE",
		    chill_predefined_function_type,
		    BUILT_IN_OUTOFFILE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "readable" : "READABLE",
		    chill_predefined_function_type,
		    BUILT_IN_READABLE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "readrecord" : "READRECORD",
		    chill_predefined_function_type,
		    BUILT_IN_READRECORD, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "readtext" : "READTEXT",
		    chill_predefined_function_type,
		    BUILT_IN_READTEXT, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "sequencible" : "SEQUENCIBLE",
		    chill_predefined_function_type,
		    BUILT_IN_SEQUENCIBLE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "settextaccess" : "SETTEXTACCESS",
		    chill_predefined_function_type,
		    BUILT_IN_SETTEXTACCESS, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "settextindex" : "SETTEXTINDEX",
		    chill_predefined_function_type,
		    BUILT_IN_SETTEXTINDEX, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "settextrecord" : "SETTEXTRECORD",
		    chill_predefined_function_type,
		    BUILT_IN_SETTEXTRECORD, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "variable" : "VARIABLE",
		    chill_predefined_function_type,
		    BUILT_IN_VARIABLE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "writeable" : "WRITEABLE",
		    chill_predefined_function_type,
		    BUILT_IN_WRITEABLE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "writerecord" : "WRITERECORD",
		    chill_predefined_function_type,
		    BUILT_IN_WRITERECORD, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "writetext" : "WRITETEXT",
		    chill_predefined_function_type,
		    BUILT_IN_WRITETEXT, BUILT_IN_NORMAL, NULL_PTR);

  /* build function prototypes */
  bool_ftype_ptr_ptr_int = 
    build_function_type (boolean_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            endlink))));
  ptr_ftype_ptr_ptr_int_ptr_int_ptr_int = 
    build_function_type (ptr_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            tree_cons (NULL_TREE, ptr_type_node,
              tree_cons (NULL_TREE, integer_type_node,
                tree_cons (NULL_TREE, ptr_type_node,
                  tree_cons (NULL_TREE, integer_type_node,
		    endlink))))))));
  void_ftype_ptr_ptr_int = 
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            endlink))));
  void_ftype_ptr_ptr_int_ptr_int_ptr_int = 
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            tree_cons (NULL_TREE, ptr_type_node,
              tree_cons (NULL_TREE, integer_type_node,
                tree_cons (NULL_TREE, ptr_type_node,
                  tree_cons (NULL_TREE, integer_type_node,
		    endlink))))))));
  void_ftype_ptr_ptr_int_int_int_long_ptr_int =
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            tree_cons (NULL_TREE, integer_type_node,
              tree_cons (NULL_TREE, integer_type_node,
                tree_cons (NULL_TREE, long_integer_type_node,
                  tree_cons (NULL_TREE, ptr_type_node,
                    tree_cons (NULL_TREE, integer_type_node,
                      endlink)))))))));
  ptr_ftype_ptr_ptr_int = 
    build_function_type (ptr_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            endlink))));
  int_ftype_ptr_ptr_int = 
    build_function_type (integer_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            endlink))));
  ptr_ftype_ptr_int_ptr_ptr_int = 
    build_function_type (ptr_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, ptr_type_node,
              tree_cons (NULL_TREE, integer_type_node,
                endlink))))));
  void_ftype_ptr_int_ptr_luns_ptr_int = 
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, long_unsigned_type_node,
              tree_cons (NULL_TREE, ptr_type_node,
                tree_cons (NULL_TREE, integer_type_node,
                  endlink)))))));
  luns_ftype_ptr_ptr_int = 
    build_function_type (long_unsigned_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, integer_type_node,
            endlink))));
  void_ftype_ptr_ptr_ptr_int = 
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, ptr_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, integer_type_node,
              endlink)))));
  void_ftype_ptr_int_ptr_int = 
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, integer_type_node,
              endlink)))));
  void_ftype_ptr_int_ptr_int_ptr_int_ptr_int =
    build_function_type (void_type_node,
      tree_cons (NULL_TREE, ptr_type_node,
        tree_cons (NULL_TREE, integer_type_node,
          tree_cons (NULL_TREE, ptr_type_node,
            tree_cons (NULL_TREE, integer_type_node,
              tree_cons (NULL_TREE, ptr_type_node,
                tree_cons (NULL_TREE, integer_type_node,
                  tree_cons (NULL_TREE, ptr_type_node,
                    tree_cons (NULL_TREE, integer_type_node,
                      endlink)))))))));

  builtin_function ("__associate", ptr_ftype_ptr_ptr_int_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__connect", void_ftype_ptr_ptr_int_int_int_long_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__create", void_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__delete", void_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__disconnect", void_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__dissociate", void_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__eoln", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__existing", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__getassociation", ptr_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__gettextaccess", ptr_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__gettextindex", luns_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__gettextrecord", ptr_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__getusage", int_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__indexable", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__isassociated", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__modify", void_ftype_ptr_ptr_int_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__outoffile", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__readable", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__readrecord", ptr_ftype_ptr_int_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__readtext_f", void_ftype_ptr_int_ptr_int_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__readtext_s", void_ftype_ptr_int_ptr_int_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__sequencible", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__settextaccess", void_ftype_ptr_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__settextindex", void_ftype_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__settextrecord", void_ftype_ptr_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__variable", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__writeable", bool_ftype_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__writerecord", void_ftype_ptr_int_ptr_luns_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__writetext_f", void_ftype_ptr_int_ptr_int_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__writetext_s", void_ftype_ptr_int_ptr_int_ptr_int_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);

  /* declare ASSOCIATION, ACCESS, and TEXT modes */
  build_io_types ();

  /* declare the predefined text locations */
  declare_predefined_file ((ignore_case || ! special_UC) ?  "stdin" : "STDIN",
			   "chill_stdin");
  declare_predefined_file ((ignore_case || ! special_UC) ?  "stdout" : "STDOUT",
			   "chill_stdout");
  declare_predefined_file ((ignore_case || ! special_UC) ?  "stderr" : "STDERR",
			   "chill_stderr");

  /* last, but not least, build the chill IO-list type */
  build_chill_io_list_type ();

  maximum_field_alignment = save_maximum_field_alignment;
}

/* function returns the recordmode of an ACCESS */
tree
access_recordmode (access)
     tree access;
{
  tree field;

  if (access == NULL_TREE || TREE_CODE (access) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_ACCESS_MODE (access))
    return NULL_TREE;

  field = TYPE_FIELDS (access);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == TYPE_DECL &&
	  DECL_NAME (field) == get_identifier ("__recordmode"))
	return TREE_TYPE (field);
    }
  return void_type_node;
}

/* function invalidates the recordmode of an ACCESS */
void
invalidate_access_recordmode (access)
     tree access;
{
  tree field;

  if (access == NULL_TREE || TREE_CODE (access) == ERROR_MARK)
    return;
  if (! CH_IS_ACCESS_MODE (access))
    return;

  field = TYPE_FIELDS (access);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == TYPE_DECL &&
	  DECL_NAME (field) == get_identifier ("__recordmode"))
	{
	  TREE_TYPE (field) = error_mark_node;
	  return;
	}
    }
}

/* function returns the index mode of an ACCESS if there is one,
   otherwise NULL_TREE */
tree
access_indexmode (access)
     tree access;
{
  tree field;

  if (access == NULL_TREE || TREE_CODE (access) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_ACCESS_MODE (access))
    return NULL_TREE;

  field = TYPE_FIELDS (access);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == TYPE_DECL &&
	  DECL_NAME (field) == get_identifier ("__indexmode"))
	return TREE_TYPE (field);
    }
  return void_type_node;
}

/* function returns one if an ACCESS was specified DYNAMIC, otherwise zero */
tree
access_dynamic (access)
     tree access;
{
  tree field;

  if (access == NULL_TREE || TREE_CODE (access) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_ACCESS_MODE (access))
    return NULL_TREE;

  field = TYPE_FIELDS (access);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == CONST_DECL)
	return DECL_INITIAL (field);
    }
  return integer_zero_node;
}

#if 0
   returns a structure like
   STRUCT (data STRUCT (flags ULONG,
                        reclength ULONG,
                        lowindex LONG,
                        highindex LONG,
                        association PTR,
                        base ULONG,
                        store_loc PTR,
                        rectype LONG),
   this is followed by a
   TYPE_DECL __recordmode recordmode ? recordmode : void_type_node
   TYPE_DECL __indexmode  indexmode  ? indexmode  : void_type_node
   CONST_DECL __dynamic   dynamic ? integer_one_node : integer_zero_node
#endif

static tree
build_access_part ()
{
  tree listbase, decl;

  listbase = build_decl (FIELD_DECL, get_identifier ("flags"),
			 long_unsigned_type_node);
  decl = build_decl (FIELD_DECL, get_identifier ("reclength"),
		     long_unsigned_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("lowindex"),
		     long_unsigned_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("highindex"),
		     long_integer_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("association"),
		     ptr_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("base"),
		     long_unsigned_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("storelocptr"),
		     ptr_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("rectype"),
		     long_integer_type_node);
  listbase = chainon (listbase, decl);
  return build_chill_struct_type (listbase);
}

tree
build_access_mode (indexmode, recordmode, dynamic)
     tree indexmode;
     tree recordmode;
     int dynamic;
{
  tree type, listbase, decl, datamode;

  if (indexmode != NULL_TREE && TREE_CODE (indexmode) == ERROR_MARK)
    return error_mark_node;
  if (recordmode != NULL_TREE && TREE_CODE (recordmode) == ERROR_MARK)
    return error_mark_node;

  datamode = build_access_part ();
  
  type = make_node (RECORD_TYPE);
  listbase = build_decl (FIELD_DECL, get_identifier ("data"),
			 datamode);
  TYPE_FIELDS (type) = listbase;
  decl = build_lang_decl (TYPE_DECL, get_identifier ("__recordmode"),
			  recordmode == NULL_TREE ? void_type_node : recordmode);
  chainon (listbase, decl);
  decl = build_lang_decl (TYPE_DECL, get_identifier ("__indexmode"),
			  indexmode == NULL_TREE ? void_type_node : indexmode);
  chainon (listbase, decl);
  decl = build_decl (CONST_DECL, get_identifier ("__dynamic"),
		     integer_type_node);
  DECL_INITIAL (decl) = dynamic ? integer_one_node : integer_zero_node;
  chainon (listbase, decl);
  CH_IS_ACCESS_MODE (type) = 1;
  CH_TYPE_NONVALUE_P (type) = 1;
  return type;
}

#if 0
  returns a structure like:
  STRUCT (txt STRUCT (flags ULONG,
                      text_record PTR,
                      access_sub PTR,
                      actual_index LONG),
          acc STRUCT (flags ULONG,
                      reclength ULONG,
                      lowindex LONG,
                      highindex LONG,
                      association PTR,
                      base ULONG,
                      store_loc PTR,
                      rectype LONG),
          tloc CHARS(textlength) VARYING;
          )
  followed by
  TYPE_DECL __indexmode indexmode ? indexmode : void_type_node
  CONST_DECL __text_length
  CONST_DECL __dynamic  dynamic ? integer_one_node : integer_zero_node
#endif
tree
build_text_mode (textlength, indexmode, dynamic)
     tree textlength;
     tree indexmode;
     int dynamic;
{
  tree txt, acc, listbase, decl, type, tltype;
  tree savedlength = textlength;

  if (indexmode != NULL_TREE && TREE_CODE (indexmode) == ERROR_MARK)
    return error_mark_node;
  if (textlength == NULL_TREE || TREE_CODE (textlength) == ERROR_MARK)
    return error_mark_node;

  /* build the structure */
  listbase = build_decl (FIELD_DECL, get_identifier ("flags"),
			 long_unsigned_type_node);
  decl = build_decl (FIELD_DECL, get_identifier ("text_record"),
		     ptr_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("access_sub"),
		     ptr_type_node);
  listbase = chainon (listbase, decl);
  decl = build_decl (FIELD_DECL, get_identifier ("actual_index"),
		     long_integer_type_node);
  listbase = chainon (listbase, decl);
  txt = build_chill_struct_type (listbase);

  acc = build_access_part ();

  type = make_node (RECORD_TYPE);
  listbase = build_decl (FIELD_DECL, get_identifier ("txt"), txt);
  TYPE_FIELDS (type) = listbase;
  decl = build_decl (FIELD_DECL, get_identifier ("acc"), acc);
  chainon (listbase, decl);
  /* the text location */
  tltype = build_string_type (char_type_node, textlength);
  tltype = build_varying_struct (tltype);
  decl = build_decl (FIELD_DECL, get_identifier ("tloc"),
		     tltype);
  chainon (listbase, decl);
  /* the index mode */
  decl = build_lang_decl (TYPE_DECL, get_identifier ("__indexmode"),
			  indexmode == NULL_TREE ? void_type_node : indexmode);
  chainon (listbase, decl);
  /* save dynamic */
  decl = build_decl (CONST_DECL, get_identifier ("__textlength"),
		     integer_type_node);
  if (TREE_CODE (textlength) == COMPONENT_REF)
    /* FIXME: we cannot use one and the same COMPONENT_REF twice, so build
       another one */
    savedlength = build_component_ref (TREE_OPERAND (textlength, 0),
				       TREE_OPERAND (textlength, 1));
  DECL_INITIAL (decl) = savedlength;
  chainon (listbase, decl);
  /* save dynamic */
  decl = build_decl (CONST_DECL, get_identifier ("__dynamic"),
		     integer_type_node);
  DECL_INITIAL (decl) = dynamic ? integer_one_node : integer_zero_node;
  chainon (listbase, decl);
  CH_IS_TEXT_MODE (type) = 1;
  CH_TYPE_NONVALUE_P (type) = 1;
  return type;
}

tree
check_text_length (length)
     tree length;
{
  if (length == NULL_TREE || TREE_CODE (length) == ERROR_MARK)
    return length;
  if (TREE_TYPE (length) == NULL_TREE
      || !CH_SIMILAR (TREE_TYPE (length), integer_type_node))
    {
      error ("non-integral text length");
      return integer_one_node;
    }
  if (TREE_CODE (length) != INTEGER_CST)
    {
      error ("non-constant text length");
      return integer_one_node;
    }
  if (compare_int_csts (LE_EXPR, length, integer_zero_node))
    {
      error ("text length must be greater then 0");
      return integer_one_node;
    }
  return length;
}

tree
text_indexmode (text)
     tree text;
{
  tree field;

  if (text == NULL_TREE || TREE_CODE (text) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_TEXT_MODE (text))
    return NULL_TREE;

  field = TYPE_FIELDS (text);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == TYPE_DECL)
	return TREE_TYPE (field);
    }
  return void_type_node;
}

tree
text_dynamic (text)
     tree text;
{
  tree field;

  if (text == NULL_TREE || TREE_CODE (text) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_TEXT_MODE (text))
    return NULL_TREE;

  field = TYPE_FIELDS (text);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == CONST_DECL &&
	  DECL_NAME (field) == get_identifier ("__dynamic"))
	return DECL_INITIAL (field);
    }
  return integer_zero_node;
}

tree
text_length (text)
     tree text;
{
  tree field;

  if (text == NULL_TREE || TREE_CODE (text) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_TEXT_MODE (text))
    return NULL_TREE;

  field = TYPE_FIELDS (text);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == CONST_DECL &&
	  DECL_NAME (field) == get_identifier ("__textlength"))
	return DECL_INITIAL (field);
    }
  return integer_zero_node;
}

static tree
textlocation_mode (text)
     tree text;
{
  tree field;

  if (text == NULL_TREE || TREE_CODE (text) == ERROR_MARK)
    return NULL_TREE;
  if (! CH_IS_TEXT_MODE (text))
    return NULL_TREE;

  field = TYPE_FIELDS (text);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL &&
	  DECL_NAME (field) == get_identifier ("tloc"))
	return TREE_TYPE (field);
    }
  return NULL_TREE;
}

static int
check_assoc (assoc, argnum, errmsg)
     tree assoc;
     int argnum;
     const char *errmsg;
{
  if (assoc == NULL_TREE || TREE_CODE (assoc) == ERROR_MARK)
    return 0;

  if (! CH_IS_ASSOCIATION_MODE (TREE_TYPE (assoc)))
    {
      error ("argument %d of %s must be of mode ASSOCIATION", argnum, errmsg);
      return 0;
    }
  if (! CH_LOCATION_P (assoc))
    {
      error ("argument %d of %s must be a location", argnum, errmsg);
      return 0;
    }
  return 1;
}

tree
build_chill_associate (assoc, fname, attr)
     tree assoc;
     tree fname;
     tree attr;
{
  tree arg1 = NULL_TREE, arg2 = NULL_TREE, arg3 = NULL_TREE, arg4 = NULL_TREE,
  arg5 = NULL_TREE, arg6, arg7;
  int had_errors = 0;
  tree result;

  /* make some checks */
  if (fname == NULL_TREE || TREE_CODE (fname) == ERROR_MARK)
    return error_mark_node;

  /* check the association */
  if (! check_assoc (assoc, 1, "ASSOCIATION"))
    had_errors = 1;
  else
    /* build a pointer to the association */
    arg1 = force_addr_of (assoc);

  /* check the filename, must be a string */
  if (CH_CHARS_TYPE_P (TREE_TYPE (fname)) ||
      (flag_old_strings && TREE_CODE (fname) == INTEGER_CST &&
       TREE_CODE (TREE_TYPE (fname)) == CHAR_TYPE))
    {
      if (int_size_in_bytes (TREE_TYPE (fname)) == 0)
	{
	  error ("argument 2 of ASSOCIATE must not be an empty string");
	  had_errors = 1;
	}
      else
	{
	  arg2 = force_addr_of (fname);
	  arg3 = size_in_bytes (TREE_TYPE (fname));
	}
    }
  else if (chill_varying_string_type_p (TREE_TYPE (fname)))
    {
      arg2 = force_addr_of (build_component_ref (fname, var_data_id));
      arg3 = build_component_ref (fname, var_length_id);
    }
  else
    {
      error ("argument 2 to ASSOCIATE must be a string");
      had_errors = 1;
    }

  /* check attr argument, must be a string too */
  if (attr == NULL_TREE)
    {
      arg4 = null_pointer_node;
      arg5 = integer_zero_node;
    }
  else
    {
      attr = TREE_VALUE (attr);
      if (attr == NULL_TREE || TREE_CODE (attr) == ERROR_MARK)
	had_errors = 1;
      else
	{
	  if (CH_CHARS_TYPE_P (TREE_TYPE (attr)) ||
	      (flag_old_strings && TREE_CODE (attr) == INTEGER_CST &&
	       TREE_CODE (TREE_TYPE (attr)) == CHAR_TYPE))
	    {
	      if (int_size_in_bytes (TREE_TYPE (attr)) == 0)
		{
		  arg4 = null_pointer_node;
		  arg5 = integer_zero_node;
		}
	      else
		{
		  arg4 = force_addr_of (attr);
		  arg5 = size_in_bytes (TREE_TYPE (attr));
		}
	    }
	  else if (chill_varying_string_type_p (TREE_TYPE (attr)))
	    {
	      arg4 = force_addr_of (build_component_ref (attr, var_data_id));
	      arg5 = build_component_ref (attr, var_length_id);
	    }
	  else
	    {
	      error ("argument 3 to ASSOCIATE must be a string");
	      had_errors = 1;
	    }
	}
    }

  if (had_errors)
    return error_mark_node;

  /* other arguments */
  arg6 = force_addr_of (get_chill_filename ());
  arg7 = get_chill_linenumber ();

  result = build_chill_function_call (
     lookup_name (get_identifier ("__associate")),
            tree_cons (NULL_TREE, arg1,
              tree_cons (NULL_TREE, arg2,
                tree_cons (NULL_TREE, arg3,
                  tree_cons (NULL_TREE, arg4,
                    tree_cons (NULL_TREE, arg5,
                      tree_cons (NULL_TREE, arg6,
                        tree_cons (NULL_TREE, arg7, NULL_TREE))))))));
  
  TREE_TYPE (result) = build_chill_pointer_type (TREE_TYPE (assoc));
  return result;
}

static tree
assoc_call (assoc, func, name)
     tree assoc;
     tree func;
     const char *name;
{
  tree arg1, arg2, arg3;
  tree result;

  if (! check_assoc (assoc, 1, name))
    return error_mark_node;

  arg1 = force_addr_of (assoc);
  arg2 = force_addr_of (get_chill_filename ());
  arg3 = get_chill_linenumber ();

  result = build_chill_function_call (func,
            tree_cons (NULL_TREE, arg1,
              tree_cons (NULL_TREE, arg2,
                tree_cons (NULL_TREE, arg3, NULL_TREE))));
  return result;
}

tree
build_chill_isassociated (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__isassociated")),
			    "ISASSOCIATED");
  return result;
}

tree
build_chill_existing (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__existing")),
			    "EXISTING");
  return result;
}

tree
build_chill_readable (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__readable")),
			    "READABLE");
  return result;
}

tree
build_chill_writeable (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__writeable")),
			    "WRITEABLE");
  return result;
}

tree
build_chill_sequencible (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__sequencible")),
			    "SEQUENCIBLE");
  return result;
}

tree
build_chill_variable (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__variable")),
			    "VARIABLE");
  return result;
}

tree
build_chill_indexable (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__indexable")),
			    "INDEXABLE");
  return result;
}

tree
build_chill_dissociate (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__dissociate")),
			    "DISSOCIATE");
  return result;
}

tree
build_chill_create (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__create")),
			    "CREATE");
  return result;
}

tree
build_chill_delete (assoc)
     tree assoc;
{
  tree result = assoc_call (assoc,
			    lookup_name (get_identifier ("__delete")),
			    "DELETE");
  return result;
}

tree
build_chill_modify (assoc, list)
     tree assoc;
     tree list;
{
  tree arg1 = NULL_TREE, arg2 = NULL_TREE, arg3 = NULL_TREE, arg4 = NULL_TREE,
  arg5 = NULL_TREE, arg6, arg7;
  int had_errors = 0, numargs;
  tree fname = NULL_TREE, attr = NULL_TREE;
  tree result;

  /* check the association */
  if (! check_assoc (assoc, 1, "MODIFY"))
    had_errors = 1;
  else
    arg1 = force_addr_of (assoc);

  /* look how much arguments we have got */
  numargs = list_length (list);
  switch (numargs)
    {
    case 0:
      break;
    case 1:
      fname = TREE_VALUE (list);
      break;
    case 2:
      fname = TREE_VALUE (list);
      attr = TREE_VALUE (TREE_CHAIN (list));
      break;
    default:
      error ("Too many arguments in call to MODIFY");
      had_errors = 1;
      break;
    }

  if (fname !=  NULL_TREE && fname != null_pointer_node)
    {
      if (CH_CHARS_TYPE_P (TREE_TYPE (fname)) ||
	  (flag_old_strings && TREE_CODE (fname) == INTEGER_CST &&
	   TREE_CODE (TREE_TYPE (fname)) == CHAR_TYPE))
	{
	  if (int_size_in_bytes (TREE_TYPE (fname)) == 0)
	    {
	      error ("argument 2 of MODIFY must not be an empty string");
	      had_errors = 1;
	    }
	  else
	    {
	      arg2 = force_addr_of (fname);
	      arg3 = size_in_bytes (TREE_TYPE (fname));
	    }
	}
      else if (chill_varying_string_type_p (TREE_TYPE (fname)))
	{
	  arg2 = force_addr_of (build_component_ref (fname, var_data_id));
	  arg3 = build_component_ref (fname, var_length_id);
	}
      else
	{
	  error ("argument 2 to MODIFY must be a string");
	  had_errors = 1;
	}
    }
  else
    {
      arg2 = null_pointer_node;
      arg3 = integer_zero_node;
    }

  if (attr != NULL_TREE && attr != null_pointer_node)
    {
      if (CH_CHARS_TYPE_P (TREE_TYPE (attr)) ||
	  (flag_old_strings && TREE_CODE (attr) == INTEGER_CST &&
	   TREE_CODE (TREE_TYPE (attr)) == CHAR_TYPE))
	{
	  if (int_size_in_bytes (TREE_TYPE (attr)) == 0)
	    {
	      arg4 = null_pointer_node;
	      arg5 = integer_zero_node;
	    }
	  else
	    {
	      arg4 = force_addr_of (attr);
	      arg5 = size_in_bytes (TREE_TYPE (attr));
	    }
	}
      else if (chill_varying_string_type_p (TREE_TYPE (attr)))
	{
	  arg4 = force_addr_of (build_component_ref (attr, var_data_id));
	  arg5 = build_component_ref (attr, var_length_id);
	}
      else
	{
	  error ("argument 3 to MODIFY must be a string");
	  had_errors = 1;
	}
    }
  else
    {
      arg4 = null_pointer_node;
      arg5 = integer_zero_node;
    }

  if (had_errors)
    return error_mark_node;

  /* other arguments */
  arg6 = force_addr_of (get_chill_filename ());
  arg7 = get_chill_linenumber ();

  result = build_chill_function_call (
     lookup_name (get_identifier ("__modify")),
            tree_cons (NULL_TREE, arg1,
              tree_cons (NULL_TREE, arg2,
                tree_cons (NULL_TREE, arg3,
                  tree_cons (NULL_TREE, arg4,
                    tree_cons (NULL_TREE, arg5,
                      tree_cons (NULL_TREE, arg6,
                        tree_cons (NULL_TREE, arg7, NULL_TREE))))))));
  
  return result;
}

static int
check_transfer (transfer, argnum, errmsg)
     tree transfer;
     int argnum;
     const char *errmsg;
{
  int result = 0;

  if (transfer == NULL_TREE || TREE_CODE (transfer) == ERROR_MARK)
    return 0;

  if (CH_IS_ACCESS_MODE (TREE_TYPE (transfer)))
    result = 1;
  else if (CH_IS_TEXT_MODE (TREE_TYPE (transfer)))
    result = 2;
  else
    {
      error ("argument %d of %s must be an ACCESS or TEXT mode", argnum, errmsg);
      return 0;
    }
  if (! CH_LOCATION_P (transfer))
    {
      error ("argument %d of %s must be a location", argnum, errmsg);
      return 0;
    }
  return result;
}

/* define bits in an access/text flag word.
   NOTE: this must be consistent with runtime/iomodes.h */
#define IO_TEXTLOCATION 0x80000000
#define IO_INDEXED      0x00000001
#define IO_TEXTIO       0x00000002
#define IO_OUTOFFILE    0x00010000

/* generated initialisation code for ACCESS and TEXT.
   functions gets called from do_decl. */
void init_access_location (decl, type)
     tree decl;
     tree type;
{
  tree recordmode = access_recordmode (type);
  tree indexmode = access_indexmode (type);
  int flags_init = 0;
  tree data = build_component_ref (decl, get_identifier ("data"));
  tree lowindex = integer_zero_node;
  tree highindex = integer_zero_node;
  tree rectype, reclen;

  /* flag word */
  if (indexmode != NULL_TREE && indexmode != void_type_node)
    {
      flags_init |= IO_INDEXED;
      lowindex = convert (integer_type_node, TYPE_MIN_VALUE (indexmode));
      highindex = convert (integer_type_node, TYPE_MAX_VALUE (indexmode));
    }

  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("flags")),
        build_int_2 (flags_init, 0)));

  /* record length */
  if (recordmode == NULL_TREE || recordmode == void_type_node)
    {
      reclen = integer_zero_node;
      rectype = integer_zero_node;
    }
  else if (chill_varying_string_type_p (recordmode))
    {
      tree fields = TYPE_FIELDS (recordmode);
      tree len1, len2;

      /* don't count any padding bytes at end of varying */
      len1 = size_in_bytes (TREE_TYPE (fields));
      fields = TREE_CHAIN (fields);
      len2 = size_in_bytes (TREE_TYPE (fields));
      reclen = fold (build (PLUS_EXPR, long_integer_type_node, len1, len2));
      rectype = build_int_2 (2, 0);
    }
  else
    {
      reclen = size_in_bytes (recordmode);
      rectype = integer_one_node;
    }
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("reclength")), reclen));

  /* record type */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("rectype")), rectype));

  /* the index */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("lowindex")), lowindex));
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("highindex")), highindex));

  /* association */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_chill_component_ref (data, get_identifier ("association")),
        null_pointer_node));

  /* storelocptr */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("storelocptr")), null_pointer_node));
}

void init_text_location (decl, type)
     tree decl;
     tree type;
{
  tree indexmode = text_indexmode (type);
  unsigned long accessflags = 0;
  unsigned long textflags = IO_TEXTLOCATION;
  tree lowindex = integer_zero_node;
  tree highindex = integer_zero_node;
  tree data, tloc, tlocfields, len1, len2, reclen;

  if (indexmode != NULL_TREE && indexmode != void_type_node)
    {
      accessflags |= IO_INDEXED;
      lowindex = convert (integer_type_node, TYPE_MIN_VALUE (indexmode));
      highindex = convert (integer_type_node, TYPE_MAX_VALUE (indexmode));
    }

  tloc = build_component_ref (decl, get_identifier ("tloc"));
  /* fill access part of text location */
  data = build_component_ref (decl, get_identifier ("acc"));
  /* flag word */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("flags")),
        build_int_2 (accessflags, 0)));

  /* record length, don't count any padding bytes at end of varying */
  tlocfields = TYPE_FIELDS (TREE_TYPE (tloc));
  len1 = size_in_bytes (TREE_TYPE (tlocfields));
  tlocfields = TREE_CHAIN (tlocfields);
  len2 = size_in_bytes (TREE_TYPE (tlocfields));
  reclen = fold (build (PLUS_EXPR, long_integer_type_node, len1, len2));
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("reclength")),
        reclen));

  /* the index */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("lowindex")), lowindex));
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("highindex")), highindex));

  /* association */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_chill_component_ref (data, get_identifier ("association")),
        null_pointer_node));

  /* storelocptr */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("storelocptr")),
        null_pointer_node));

  /* record type */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("rectype")),
        build_int_2 (2, 0))); /* VaryingChars */

  /* fill text part */
  data = build_component_ref (decl, get_identifier ("txt"));
  /* flag word */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("flags")),
        build_int_2 (textflags, 0)));

  /* pointer to text record */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("text_record")),
        force_addr_of (tloc)));

  /* pointer to the access */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("access_sub")),
        force_addr_of (build_component_ref (decl, get_identifier ("acc")))));

  /* actual length */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (data, get_identifier ("actual_index")),
        integer_zero_node));

  /* length of text record */
  expand_expr_stmt (
    build_chill_modify_expr (
      build_component_ref (tloc, get_identifier (VAR_LENGTH)),
        integer_zero_node));
}

static int
connect_process_optionals (optionals, whereptr, indexptr, indexmode)
     tree optionals;
     tree *whereptr;
     tree *indexptr;
     tree indexmode;
{
  tree where = NULL_TREE, theindex = NULL_TREE;
  int had_errors = 0;

  if (optionals != NULL_TREE)
    {
      /* get the where expression */
      where = TREE_VALUE (optionals);
      if (where == NULL_TREE || TREE_CODE (where) == ERROR_MARK)
	had_errors = 1;
      else
	{
	  if (! CH_IS_WHERE_MODE (TREE_TYPE (where)))
	    {
	      error ("argument 4 of CONNECT must be of mode WHERE");
	      had_errors = 1;
	    }
	  where = convert (integer_type_node, where);
	}
      optionals = TREE_CHAIN (optionals);
    }
  if (optionals != NULL_TREE)
    {
      theindex = TREE_VALUE (optionals);
      if (theindex == NULL_TREE || TREE_CODE (theindex) == ERROR_MARK)
	had_errors = 1;
      else
	{
	  if (indexmode == void_type_node)
	    {
	      error ("index expression for ACCESS without index");
	      had_errors = 1;
	    }
	  else if (! CH_COMPATIBLE (theindex, indexmode))
	    {
	      error ("incompatible index mode");
	      had_errors = 1;
	    }
	}
    }
  if (had_errors)
    return 0;

  *whereptr = where;
  *indexptr = theindex;
  return 1;
}

static tree
connect_text (assoc, text, usage, optionals)
     tree assoc;
     tree text;
     tree usage;
     tree optionals;
{
  tree where = NULL_TREE, theindex = NULL_TREE;
  tree indexmode = text_indexmode (TREE_TYPE (text));
  tree result, what_where, have_index, what_index;

  /* process optionals */
  if (!connect_process_optionals (optionals, &where, &theindex, indexmode))
    return error_mark_node;

  what_where = where == NULL_TREE ? integer_zero_node : where;
  have_index = theindex == NULL_TREE ? integer_zero_node
                                     : integer_one_node;
  what_index = theindex == NULL_TREE ? integer_zero_node
                                     : convert (integer_type_node, theindex);
  result = build_chill_function_call (
             lookup_name (get_identifier ("__connect")),
               tree_cons (NULL_TREE, force_addr_of (text),
                 tree_cons (NULL_TREE, force_addr_of (assoc),
                   tree_cons (NULL_TREE, convert (integer_type_node, usage),
                     tree_cons (NULL_TREE, what_where,
                       tree_cons (NULL_TREE, have_index,
		 	 tree_cons (NULL_TREE, what_index,
                           tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                             tree_cons (NULL_TREE, get_chill_linenumber (),
					NULL_TREE)))))))));
  return result;
}

static tree
connect_access (assoc, transfer, usage, optionals)
     tree assoc;
     tree transfer;
     tree usage;
     tree optionals;
{
  tree where = NULL_TREE, theindex = NULL_TREE;
  tree indexmode = access_indexmode (TREE_TYPE (transfer));
  tree result, what_where, have_index, what_index;

  /* process the optionals */
  if (! connect_process_optionals (optionals, &where, &theindex, indexmode))
    return error_mark_node;

  /* now the call */
  what_where = where == NULL_TREE ? integer_zero_node : where;
  have_index = theindex == NULL_TREE ? integer_zero_node : integer_one_node;
  what_index = theindex == NULL_TREE ? integer_zero_node : convert (integer_type_node, theindex);
  result = build_chill_function_call (
             lookup_name (get_identifier ("__connect")),
               tree_cons (NULL_TREE, force_addr_of (transfer),
                 tree_cons (NULL_TREE, force_addr_of (assoc),
                   tree_cons (NULL_TREE, convert (integer_type_node, usage),
                     tree_cons (NULL_TREE, what_where,
                       tree_cons (NULL_TREE, have_index,
			 tree_cons (NULL_TREE, what_index,
                           tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                             tree_cons (NULL_TREE, get_chill_linenumber (),
					NULL_TREE)))))))));
  return result;
}

tree
build_chill_connect (transfer, assoc, usage, optionals)
     tree transfer;
     tree assoc;
     tree usage;
     tree optionals;
{
  int had_errors = 0;
  int what = 0;
  tree result = error_mark_node;

  if (! check_assoc (assoc, 2, "CONNECT"))
    had_errors = 1;

  /* check usage */
  if (usage == NULL_TREE || TREE_CODE (usage) == ERROR_MARK)
    return error_mark_node;

  if (! CH_IS_USAGE_MODE (TREE_TYPE (usage)))
    {
      error ("argument 3 to CONNECT must be of mode USAGE");
      had_errors = 1;
    }
  if (had_errors)
    return error_mark_node;

  /* look what we have got */
  what = check_transfer (transfer, 1, "CONNECT");
  switch (what)
    {
    case 1:
      /* we have an ACCESS */
      result = connect_access (assoc, transfer, usage, optionals);
      break;
    case 2:
      /* we have a TEXT */
      result = connect_text (assoc, transfer, usage, optionals);
      break;
    default:
      result = error_mark_node;
    }
  return result;
}

static int
check_access (access, argnum, errmsg)
     tree access;
     int argnum;
     const char *errmsg;
{
  if (access == NULL_TREE || TREE_CODE (access) == ERROR_MARK)
    return 1;

  if (! CH_IS_ACCESS_MODE (TREE_TYPE (access)))
    {
      error ("argument %d of %s must be of mode ACCESS", argnum, errmsg);
      return 0;
    }
  if (! CH_LOCATION_P (access))
    {
      error ("argument %d of %s must be a location", argnum, errmsg);
      return 0;
    }
  return 1;
}

tree
build_chill_readrecord (access, optionals)
     tree access;
     tree optionals;
{
  int len;
  tree recordmode, indexmode, dynamic, result;
  tree index = NULL_TREE, location = NULL_TREE;

  if (! check_access (access, 1, "READRECORD"))
    return error_mark_node;

  recordmode = access_recordmode (TREE_TYPE (access));
  indexmode = access_indexmode (TREE_TYPE (access));
  dynamic = access_dynamic (TREE_TYPE (access));

  /* process the optionals */
  len = list_length (optionals);
  if (indexmode != void_type_node)
    {
      /* we must have an index */
      if (!len)
	{
	  error ("Too few arguments in call to `readrecord'");
	  return error_mark_node;
	}
      index = TREE_VALUE (optionals);
      if (index == NULL_TREE || TREE_CODE (index) == ERROR_MARK)
	return error_mark_node;
      optionals = TREE_CHAIN (optionals);
      if (! CH_COMPATIBLE (index, indexmode))
	{
	  error ("incompatible index mode");
	  return error_mark_node;
	}
    }

  /* check the record mode, if one */
  if (optionals != NULL_TREE)
    {
      location = TREE_VALUE (optionals);
      if (location == NULL_TREE || TREE_CODE (location) == ERROR_MARK)
	return error_mark_node;
      if (recordmode != void_type_node &&
	  ! CH_COMPATIBLE (location, recordmode))
	{

	  error ("incompatible record mode");
	  return error_mark_node;
	}
      if (TYPE_READONLY_PROPERTY (TREE_TYPE (location)))
	{
	  error ("store location must not be READonly");
	  return error_mark_node;
	}
      location = force_addr_of (location);
    }
  else
    location = null_pointer_node;

  index = index == NULL_TREE ? integer_zero_node : convert (integer_type_node, index);
  result = build_chill_function_call (
            lookup_name (get_identifier ("__readrecord")),
              tree_cons (NULL_TREE, force_addr_of (access),
                tree_cons (NULL_TREE, index,
                  tree_cons (NULL_TREE, location,
                    tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                      tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))))));

  TREE_TYPE (result) = build_chill_pointer_type (recordmode);
  return result;
}

tree
build_chill_writerecord (access, optionals)
     tree access;
     tree optionals;
{
  int had_errors = 0, len;
  tree recordmode, indexmode, dynamic;
  tree index = NULL_TREE, location = NULL_TREE;
  tree result;

  if (! check_access (access, 1, "WRITERECORD"))
    return error_mark_node;

  recordmode = access_recordmode (TREE_TYPE (access));
  indexmode = access_indexmode (TREE_TYPE (access));
  dynamic = access_dynamic (TREE_TYPE (access));

  /* process the optionals */
  len = list_length (optionals);
  if (indexmode != void_type_node && len != 2)
    {
      error ("Too few arguments in call to `writerecord'");
      return error_mark_node;
    }
  if (indexmode != void_type_node)
    {
      index = TREE_VALUE (optionals);
      if (index == NULL_TREE || TREE_CODE (index) == ERROR_MARK)
	return error_mark_node;
      location = TREE_VALUE (TREE_CHAIN (optionals));
      if (location == NULL_TREE || TREE_CODE (location) == ERROR_MARK)
	return error_mark_node;
    }
  else
    location = TREE_VALUE (optionals);

  /* check the index */
  if (indexmode != void_type_node)
    {
      if (! CH_COMPATIBLE (index, indexmode))
	{
	  error ("incompatible index mode");
	  had_errors = 1;
	}
    }
  /* check the record mode */
  if (recordmode == void_type_node)
    {
      error ("transfer to ACCESS without record mode");
      had_errors = 1;
    }
  else if (! CH_COMPATIBLE (location, recordmode))
    {
      error ("incompatible record mode");
      had_errors = 1;
    }
  if (had_errors)
    return error_mark_node;

  index = index == NULL_TREE ? integer_zero_node : convert (integer_type_node, index);

  result = build_chill_function_call (
             lookup_name (get_identifier ("__writerecord")),
               tree_cons (NULL_TREE, force_addr_of (access),
                 tree_cons (NULL_TREE, index,
                   tree_cons (NULL_TREE, force_addr_of (location),
		     tree_cons (NULL_TREE, size_in_bytes (TREE_TYPE (location)),
                       tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                         tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE)))))));
  return result;
}

tree
build_chill_disconnect (transfer)
     tree transfer;
{
  tree result;

  if (! check_transfer (transfer, 1, "DISCONNECT"))
    return error_mark_node;
  result = build_chill_function_call (
             lookup_name (get_identifier ("__disconnect")),
               tree_cons (NULL_TREE, force_addr_of (transfer),
                 tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                   tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  return result;
}

tree
build_chill_getassociation (transfer)
     tree transfer;
{
  tree result;

  if (! check_transfer (transfer, 1, "GETASSOCIATION"))
    return error_mark_node;

  result = build_chill_function_call (
            lookup_name (get_identifier ("__getassociation")),
              tree_cons (NULL_TREE, force_addr_of (transfer),
                tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                  tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  TREE_TYPE (result) = build_chill_pointer_type (association_type_node);
  return result;
}

tree
build_chill_getusage (transfer)
     tree transfer;
{
  tree result;

  if (! check_transfer (transfer, 1, "GETUSAGE"))
    return error_mark_node;

  result = build_chill_function_call (
            lookup_name (get_identifier ("__getusage")),
              tree_cons (NULL_TREE, force_addr_of (transfer),
                tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                  tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  TREE_TYPE (result) = usage_type_node;
  return result;
}

tree
build_chill_outoffile (transfer)
     tree transfer;
{
  tree result;

  if (! check_transfer (transfer, 1, "OUTOFFILE"))
    return error_mark_node;

  result = build_chill_function_call (
             lookup_name (get_identifier ("__outoffile")),
               tree_cons (NULL_TREE, force_addr_of (transfer),
                 tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                   tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  return result;
}

static int
check_text (text, argnum, errmsg)
     tree text;
     int argnum;
     const char *errmsg;
{
  if (text == NULL_TREE || TREE_CODE (text) == ERROR_MARK)
    return 0;
  if (! CH_IS_TEXT_MODE (TREE_TYPE (text)))
    {
      error ("argument %d of %s must be of mode TEXT", argnum, errmsg);
      return 0;
    }
  if (! CH_LOCATION_P (text))
    {
      error ("argument %d of %s must be a location", argnum, errmsg);
      return 0;
    }
  return 1;
}

tree
build_chill_eoln (text)
     tree text;
{
  tree result;

  if (! check_text (text, 1, "EOLN"))
    return error_mark_node;

  result = build_chill_function_call (
             lookup_name (get_identifier ("__eoln")),
               tree_cons (NULL_TREE, force_addr_of (text),
                 tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                   tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  return result;
}

tree
build_chill_gettextindex (text)
     tree text;
{
  tree result;

  if (! check_text (text, 1, "GETTEXTINDEX"))
    return error_mark_node;

  result = build_chill_function_call (
             lookup_name (get_identifier ("__gettextindex")),
               tree_cons (NULL_TREE, force_addr_of (text),
                 tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                   tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  return result;
}

tree
build_chill_gettextrecord (text)
     tree text;
{
  tree textmode, result;

  if (! check_text (text, 1, "GETTEXTRECORD"))
    return error_mark_node;

  textmode = textlocation_mode (TREE_TYPE (text));
  if (textmode == NULL_TREE)
    {
      error ("TEXT doesn't have a location");  /* FIXME */
      return error_mark_node;
    }
  result = build_chill_function_call (
            lookup_name (get_identifier ("__gettextrecord")),
              tree_cons (NULL_TREE, force_addr_of (text),
                tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                  tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  TREE_TYPE (result) = build_chill_pointer_type (textmode);
  CH_DERIVED_FLAG (result) = 1;
  return result;
}

tree
build_chill_gettextaccess (text)
     tree text;
{
  tree access, refaccess, acc, decl, listbase;
  tree tlocmode, indexmode, dynamic;
  tree result;
  unsigned int save_maximum_field_alignment = maximum_field_alignment;

  if (! check_text (text, 1, "GETTEXTACCESS"))
    return error_mark_node;

  tlocmode = textlocation_mode (TREE_TYPE (text));
  indexmode = text_indexmode (TREE_TYPE (text));
  dynamic = text_dynamic (TREE_TYPE (text));

  /* we have to build a type for the access */
  acc = build_access_part ();
  access = make_node (RECORD_TYPE);
  listbase = build_decl (FIELD_DECL, get_identifier ("data"), acc);
  TYPE_FIELDS (access) = listbase;
  decl = build_lang_decl (TYPE_DECL, get_identifier ("__recordmode"),
			  tlocmode);
  chainon (listbase, decl);
  decl = build_lang_decl (TYPE_DECL, get_identifier ("__indexmode"),
			  indexmode);
  chainon (listbase, decl);
  decl = build_decl (CONST_DECL, get_identifier ("__dynamic"),
		     integer_type_node);
  DECL_INITIAL (decl) = dynamic;
  chainon (listbase, decl);
  maximum_field_alignment = 0;
  layout_chill_struct_type (access);
  maximum_field_alignment = save_maximum_field_alignment;
  CH_IS_ACCESS_MODE (access) = 1;
  CH_TYPE_NONVALUE_P (access) = 1;

  refaccess = build_chill_pointer_type (access);

  result = build_chill_function_call (
            lookup_name (get_identifier ("__gettextaccess")),
              tree_cons (NULL_TREE, force_addr_of (text),
                tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                  tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE))));
  TREE_TYPE (result) = refaccess;
  CH_DERIVED_FLAG (result) = 1;
  return result;
}

tree
build_chill_settextindex (text, expr)
     tree text;
     tree expr;
{
  tree result;

  if (! check_text (text, 1, "SETTEXTINDEX"))
    return error_mark_node;
  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;
  result = build_chill_function_call (
             lookup_name (get_identifier ("__settextindex")),
               tree_cons (NULL_TREE, force_addr_of (text),
                 tree_cons (NULL_TREE, expr,
                   tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                     tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE)))));
  return result;
}

tree
build_chill_settextaccess (text, access)
     tree text;
     tree access;
{
  tree result;
  tree textindexmode, accessindexmode;
  tree textrecordmode, accessrecordmode;

  if (! check_text (text, 1, "SETTEXTACCESS"))
    return error_mark_node;
  if (! check_access (access, 2, "SETTEXTACCESS"))
    return error_mark_node;

  textindexmode = text_indexmode (TREE_TYPE (text));
  accessindexmode = access_indexmode (TREE_TYPE (access));
  if (textindexmode != accessindexmode)
    {
      if (! chill_read_compatible (textindexmode, accessindexmode))
	{
	  error ("incompatible index mode for SETETEXTACCESS");
	  return error_mark_node;
	}
    }
  textrecordmode = textlocation_mode (TREE_TYPE (text));
  accessrecordmode = access_recordmode (TREE_TYPE (access));
  if (textrecordmode != accessrecordmode)
    {
      if (! chill_read_compatible (textrecordmode, accessrecordmode))
	{
	  error ("incompatible record mode for SETTEXTACCESS");
	  return error_mark_node;
	}
    }
  result = build_chill_function_call (
             lookup_name (get_identifier ("__settextaccess")),
               tree_cons (NULL_TREE, force_addr_of (text),
                 tree_cons (NULL_TREE, force_addr_of (access),
                   tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                     tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE)))));
  return result;
}

tree
build_chill_settextrecord (text, charloc)
     tree text;
     tree charloc;
{
  tree result;
  int had_errors = 0;
  tree tlocmode;

  if (! check_text (text, 1, "SETTEXTRECORD"))
    return error_mark_node;
  if (charloc == NULL_TREE || TREE_CODE (charloc) == ERROR_MARK)
    return error_mark_node;

  /* check the location */
  if (! CH_LOCATION_P (charloc))
    {
      error ("parameter 2 must be a location");
      return error_mark_node;
    }
  tlocmode = textlocation_mode (TREE_TYPE (text));
  if (! chill_varying_string_type_p (TREE_TYPE (charloc)))
    had_errors = 1;
  else if (int_size_in_bytes (tlocmode) != int_size_in_bytes (TREE_TYPE (charloc)))
    had_errors = 1;
  if (had_errors)
    {
      error ("incompatible modes in parameter 2");
      return error_mark_node;
    }
  result = build_chill_function_call (
             lookup_name (get_identifier ("__settextrecord")),
               tree_cons (NULL_TREE, force_addr_of (text),
                 tree_cons (NULL_TREE, force_addr_of (charloc),
                   tree_cons (NULL_TREE, force_addr_of (get_chill_filename ()),
                     tree_cons (NULL_TREE, get_chill_linenumber (), NULL_TREE)))));
  return result;
}

/* process iolist for READ- and WRITETEXT */

/* function walks through types as long as they are ranges,
   returns the type and min- and max-value form starting type.
   */

static tree
get_final_type_and_range (item, low, high)
     tree  item;
     tree *low;
     tree *high;
{
  tree	wrk = item;
    
  *low = TYPE_MIN_VALUE (wrk);
  *high = TYPE_MAX_VALUE (wrk);
  while (TREE_CODE (wrk) == INTEGER_TYPE &&
	 TREE_TYPE (wrk) != NULL_TREE &&
	 TREE_CODE (TREE_TYPE (wrk)) == INTEGER_TYPE &&
	 TREE_TYPE (TREE_TYPE (wrk)) != NULL_TREE)
    wrk = TREE_TYPE (wrk);
    
  return (TREE_TYPE (wrk));
}

static void
process_io_list (exprlist, iolist_addr, iolist_length, iolist_rtx, do_read,
		 argoffset)
     tree exprlist;
     tree *iolist_addr;
     tree *iolist_length;
     rtx *iolist_rtx;
     int do_read;
     int argoffset;
{
  tree idxlist;
  int idxcnt;
  int iolen;
  tree iolisttype, iolist;

  if (exprlist == NULL_TREE)
    return;
  
  iolen = list_length (exprlist);
  
  /* build indexlist for the io list */
  idxlist = build_tree_list (NULL_TREE,
			     build_chill_range_type (NULL_TREE,
						     integer_one_node,
						     build_int_2 (iolen, 0)));
  
  /* build the io-list type */
  iolisttype = build_chill_array_type (TREE_TYPE (chill_io_list_type), 
				       idxlist, 0, NULL_TREE);
  
  /* declare the iolist */
  iolist = build_decl (VAR_DECL, get_unique_identifier (do_read ? "RDTEXT" : "WRTEXT"),
		       iolisttype);
  
  /* we want to get a variable which gets marked unused after
     the function call, This is a little bit tricky cause the 
     address of this variable will be taken and therefor the variable
     gets moved out one level. However, we REALLY don't need this
     variable again. Solution: push 2 levels and do pop and free
     twice at the end. */
  push_temp_slots ();
  push_temp_slots ();
  *iolist_rtx = assign_temp (TREE_TYPE (iolist), 0, 1, 0);
  DECL_RTL (iolist) = *iolist_rtx;

  /* process the exprlist */
  idxcnt = 1;
  while (exprlist != NULL_TREE)
    {
      tree item = TREE_VALUE (exprlist);
      tree idx = build_int_2 (idxcnt++, 0);
      const char *fieldname = 0;
      const char *enumname = 0;
      tree array_ref = build_chill_array_ref_1 (iolist, idx);
      tree item_type;
      tree range_low = NULL_TREE, range_high = NULL_TREE;
      int have_range = 0;
      tree item_addr = null_pointer_node;
      int referable = 0;
      int readonly = 0;

      /* next value in exprlist */
      exprlist = TREE_CHAIN (exprlist);
      if (item == NULL_TREE || TREE_CODE (item) == ERROR_MARK)
	continue;

      item_type = TREE_TYPE (item);
      if (item_type == NULL_TREE)
	{
	  if (TREE_CODE (item) == COND_EXPR || TREE_CODE (item) == CASE_EXPR)
	    error ("conditional expression not allowed in this context");
	  else
	    error ("untyped expression as argument %d", idxcnt + 1 + argoffset);
	  continue;
	}
      else if (TREE_CODE (item_type) == ERROR_MARK)
	continue;
	  
      if (TREE_CODE (item_type) == REFERENCE_TYPE)
	{
	  item_type = TREE_TYPE (item_type);
	  item = convert (item_type, item);
	}

      /* check for a range */
      if (TREE_CODE (item_type) == INTEGER_TYPE &&
	  TREE_TYPE (item_type) != NULL_TREE)
	{
	  /* we have a range. NOTE, however, on writetext we don't process ranges  */
	  item_type = get_final_type_and_range (item_type,
						&range_low, &range_high);
	  have_range = 1;
	}

      readonly = TYPE_READONLY_PROPERTY (item_type);
      referable = CH_REFERABLE (item);
      if (referable)
	item_addr = force_addr_of (item);
      /* if we are in read and have readonly we can't do this */
      if (readonly && do_read)
	{
	  item_addr = null_pointer_node;
	  referable = 0;
	}

      /* process different types */
      if (TREE_CODE (item_type) == INTEGER_TYPE)
	{
	  int type_size = TREE_INT_CST_LOW (TYPE_SIZE (item_type));
	  tree to_assign = NULL_TREE;

	  if (do_read && referable)
	    {
	      /* process an integer in case of READTEXT and expression is
		 referable and not READONLY */
	      to_assign = item_addr;
	      if (have_range)
		{
		  /* do it for a range */
		  tree t, __forxx, __ptr, __low, __high;
		  tree what_upper, what_lower;

		  /* determine the name in the union of lower and upper */
		  if (TREE_UNSIGNED (item_type))
		    fieldname = "_ulong";
		  else
		    fieldname = "_slong";

		  switch (type_size)
		    {
		    case 8:
		      if (TREE_UNSIGNED (item_type))
			enumname = "__IO_UByteRangeLoc";
		      else
			enumname = "__IO_ByteRangeLoc";
		      break;
		    case 16:
		      if (TREE_UNSIGNED (item_type))
			enumname = "__IO_UIntRangeLoc";
		      else
			enumname = "__IO_IntRangeLoc";
		      break;
		    case 32:
		      if (TREE_UNSIGNED (item_type))
			enumname = "__IO_ULongRangeLoc";
		      else
			enumname = "__IO_LongRangeLoc";
		      break;
		    default:
		      error ("Cannot process %d bits integer for READTEXT argument %d.",
			     type_size, idxcnt + 1 + argoffset);
		      continue;
		    }

		  /* set up access to structure */
		  t = build_component_ref (array_ref,
					   get_identifier ("__t"));
		  __forxx = build_component_ref (t, get_identifier ("__locintrange"));
		  __ptr = build_component_ref (__forxx, get_identifier ("ptr"));
		  __low = build_component_ref (__forxx, get_identifier ("lower"));
		  what_lower = build_component_ref (__low, get_identifier (fieldname));
		  __high = build_component_ref (__forxx, get_identifier ("upper"));
		  what_upper = build_component_ref (__high, get_identifier (fieldname));

		  /* do the assignments */
		  expand_assignment (__ptr, item_addr, 0, 0);
		  expand_assignment (what_lower, range_low, 0, 0);
		  expand_assignment (what_upper, range_high, 0, 0);
		  fieldname = 0;
		}
	      else
		{
		  /* no range */
		  fieldname = "__locint";
		  switch (type_size)
		    {
		    case 8:
		      if (TREE_UNSIGNED (item_type))
			enumname = "__IO_UByteLoc";
		      else
			enumname = "__IO_ByteLoc";
		      break;
		    case 16:
		      if (TREE_UNSIGNED (item_type))
			enumname = "__IO_UIntLoc";
		      else
			enumname = "__IO_IntLoc";
		      break;
		    case 32:
		      if (TREE_UNSIGNED (item_type))
			enumname = "__IO_ULongLoc";
		      else
			enumname = "__IO_LongLoc";
		      break;
		    default:
		      error ("Cannot process %d bits integer for READTEXT argument %d.",
			     type_size, idxcnt + 1 + argoffset);
		      continue;
		    }
		}
	    }
	  else
	    {
	      /* process an integer in case of WRITETEXT */
	      to_assign = item;
	      switch (type_size)
		{
		case 8:
		  if (TREE_UNSIGNED (item_type))
		    {
		      enumname = "__IO_UByteVal";
		      fieldname = "__valubyte";
		    }
		  else
		    {
		      enumname = "__IO_ByteVal";
		      fieldname = "__valbyte";
		    }
		  break;
		case 16:
		  if (TREE_UNSIGNED (item_type))
		    {
		      enumname = "__IO_UIntVal";
		      fieldname = "__valuint";
		    }
		  else
		    {
		      enumname = "__IO_IntVal";
		      fieldname = "__valint";
		    }
		  break;
		case 32:
		try_long:
		  if (TREE_UNSIGNED (item_type))
		    {
		      enumname = "__IO_ULongVal";
		      fieldname = "__valulong";
		    }
		  else
		    {
		      enumname = "__IO_LongVal";
		      fieldname = "__vallong";
		    }
		  break;
		case 64:
		  /* convert it back to {unsigned}long. */
		  if (TREE_UNSIGNED (item_type))
		    item_type = long_unsigned_type_node;
		  else
		    item_type = long_integer_type_node;
		  item = convert (item_type, item);
		  goto try_long;
		default:
		  /* This kludge is because the lexer gives literals
		     the type long_long_{integer,unsigned}_type_node.  */
		  if (TREE_CODE (item) == INTEGER_CST)
		    {
		      if (int_fits_type_p (item, long_integer_type_node))
			{
			  item_type = long_integer_type_node;
			  item = convert (item_type, item);
			  goto try_long;
			}
		      if (int_fits_type_p (item, long_unsigned_type_node))
			{
			  item_type = long_unsigned_type_node;
			  item = convert (item_type, item);
			  goto try_long;
			}
		    }
		  error ("Cannot process %d bits integer WRITETEXT argument %d.",
			 type_size, idxcnt + 1 + argoffset);
		  continue;
		}
	    }
	  if (fieldname)
	    {
	      tree	t, __forxx;
	      
	      t = build_component_ref (array_ref,
				       get_identifier ("__t"));
	      __forxx = build_component_ref (t, get_identifier (fieldname));
	      expand_assignment (__forxx, to_assign, 0, 0);
	    }
	}
      else if (TREE_CODE (item_type) == CHAR_TYPE)
	{
	  tree	to_assign = NULL_TREE;

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read)
	    {
	      if (! referable)
		{
		  error ("argument %d must be referable", idxcnt + 1 + argoffset);
		  continue;
		}
	      if (have_range)
		{
		  tree t, forxx, ptr, lower, upper;

		  t = build_component_ref (array_ref, get_identifier ("__t"));
		  forxx = build_component_ref (t, get_identifier ("__loccharrange"));
		  ptr = build_component_ref (forxx, get_identifier ("ptr"));
		  lower = build_component_ref (forxx, get_identifier ("lower"));
		  upper = build_component_ref (forxx, get_identifier ("upper"));
		  expand_assignment (ptr, item_addr, 0, 0);
		  expand_assignment (lower, range_low, 0, 0);
		  expand_assignment (upper, range_high, 0, 0);

		  fieldname = 0;
		  enumname = "__IO_CharRangeLoc";
		}
	      else
		{
		  to_assign = item_addr;
		  fieldname = "__locchar";
		  enumname = "__IO_CharLoc";
		}
	    }
	  else
	    {
	      to_assign = item;
	      enumname = "__IO_CharVal";
	      fieldname = "__valchar";
	    }
	  
	  if (fieldname)
	    {
	      tree t, forxx;

	      t = build_component_ref (array_ref, get_identifier ("__t"));
	      forxx = build_component_ref (t, get_identifier (fieldname));
	      expand_assignment (forxx, to_assign, 0, 0);
	    }
	}
      else if (TREE_CODE (item_type) == BOOLEAN_TYPE)
	{
	  tree to_assign = NULL_TREE;

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read)
	    {
	      if (! referable)
		{
		  error ("argument %d must be referable", idxcnt + 1 + argoffset);
		  continue;
		}
	      if (have_range)
		{
		  tree t, forxx, ptr, lower, upper;

		  t = build_component_ref (array_ref, get_identifier ("__t"));
		  forxx = build_component_ref (t, get_identifier ("__locboolrange"));
		  ptr = build_component_ref (forxx, get_identifier ("ptr"));
		  lower = build_component_ref (forxx, get_identifier ("lower"));
		  upper = build_component_ref (forxx, get_identifier ("upper"));
		  expand_assignment (ptr, item_addr, 0, 0);
		  expand_assignment (lower, range_low, 0, 0);
		  expand_assignment (upper, range_high, 0, 0);

		  fieldname = 0;
		  enumname = "__IO_BoolRangeLoc";
		}
	      else
		{
		  to_assign = item_addr;
		  fieldname = "__locbool";
		  enumname = "__IO_BoolLoc";
		}
	    }
	  else
	    {
	      to_assign = item;
	      enumname = "__IO_BoolVal";
	      fieldname = "__valbool";
	    }
	  if (fieldname)
	    {
	      tree	t, forxx;
	      
	      t = build_component_ref (array_ref, get_identifier ("__t"));
	      forxx = build_component_ref (t, get_identifier (fieldname));
	      expand_assignment (forxx, to_assign, 0, 0);
	    }
	}
      else if (TREE_CODE (item_type) == ENUMERAL_TYPE)
	{
	  /* process an enum */
	  tree table_name;
	  tree context_of_type;
	  tree t;

	  /* determine the context of the type.
	     if TYPE_NAME (item_type) == NULL_TREE
	     if TREE_CODE (item) == INTEGER_CST
	     context = NULL_TREE -- this is wrong but should work for now
	     else
	     context = DECL_CONTEXT (item)
	     else
	     context = DECL_CONTEXT (TYPE_NAME (item_type)) */

	  if (TYPE_NAME (item_type) == NULL_TREE)
	    {
	      if (TREE_CODE (item) == INTEGER_CST)
		context_of_type = NULL_TREE;
	      else
		context_of_type = DECL_CONTEXT (item);
	    }
	  else
	    context_of_type = DECL_CONTEXT (TYPE_NAME (item_type));
	      
	  table_name = add_enum_to_list (item_type, context_of_type);
	  t = build_component_ref (array_ref, get_identifier ("__t"));

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read)
	    {
	      if (! referable)
		{
		  error ("argument %d must be referable", idxcnt + 1 + argoffset);
		  continue;
		}
	      if (have_range)
		{
		  tree forxx, ptr, len, nametable, lower, upper;

		  forxx = build_component_ref (t, get_identifier ("__locsetrange"));
		  ptr = build_component_ref (forxx, get_identifier ("ptr"));
		  len = build_component_ref (forxx, get_identifier ("length"));
		  nametable = build_component_ref (forxx, get_identifier ("name_table"));
		  lower = build_component_ref (forxx, get_identifier ("lower"));
		  upper = build_component_ref (forxx, get_identifier ("upper"));
		  expand_assignment (ptr, item_addr, 0, 0);
		  expand_assignment (len, size_in_bytes (item_type), 0, 0);
		  expand_assignment (nametable, table_name, 0, 0);
		  expand_assignment (lower, range_low, 0, 0);
		  expand_assignment (upper, range_high, 0, 0);

		  enumname = "__IO_SetRangeLoc";
		}
	      else
		{
		  tree forxx, ptr, len, nametable;

		  forxx = build_component_ref (t, get_identifier ("__locset"));
		  ptr = build_component_ref (forxx, get_identifier ("ptr"));
		  len = build_component_ref (forxx, get_identifier ("length"));
		  nametable = build_component_ref (forxx, get_identifier ("name_table"));
		  expand_assignment (ptr, item_addr, 0, 0);
		  expand_assignment (len, size_in_bytes (item_type), 0, 0);
		  expand_assignment (nametable, table_name, 0, 0);

		  enumname = "__IO_SetLoc";
		}
	    }
	  else
	    {
	      tree forxx, value, nametable;

	      forxx = build_component_ref (t, get_identifier ("__valset"));
	      value = build_component_ref (forxx, get_identifier ("value"));
	      nametable = build_component_ref (forxx, get_identifier ("name_table"));
	      expand_assignment (value, item, 0, 0);
	      expand_assignment (nametable, table_name, 0, 0);

	      enumname = "__IO_SetVal";
	    }
	}
      else if (chill_varying_string_type_p (item_type))
	{
	  /* varying char string */
	  tree t = build_component_ref (array_ref, get_identifier ("__t"));
	  tree forxx = build_component_ref (t, get_identifier ("__loccharstring"));
	  tree string = build_component_ref (forxx, get_identifier ("string"));
	  tree length = build_component_ref (forxx, get_identifier ("string_length"));

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read)
	    {
	      /* in this read case the argument must be referable */
	      if (! referable)
		{
		  error ("argument %d must be referable", idxcnt + 1 + argoffset);
		  continue;
		}
	    }
	  else if (! referable)
	    {
	      /* in the write case we create a temporary if not referable */
	      rtx t;
	      tree loc = build_decl (VAR_DECL,
				     get_unique_identifier ("WRTEXTVS"),
				     item_type);
	      t = assign_temp (item_type, 0, 1, 0);
	      DECL_RTL (loc) = t;
	      expand_assignment (loc, item, 0, 0);
	      item_addr = force_addr_of (loc);
	      item = loc;
	    }

	  expand_assignment (string, item_addr, 0, 0);
	  if (do_read)
	    /* we must pass the maximum length of the varying */
	    expand_assignment (length,
			       size_in_bytes (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (item_type)))),
			       0, 0);
	  else
	      /* we pass the actual length of the string */
	    expand_assignment (length,
			       build_component_ref (item, var_length_id),
			       0, 0);

	  enumname = "__IO_CharVaryingLoc";
	}
      else if (CH_CHARS_TYPE_P (item_type))
	{
	  /* fixed character string */
	  tree the_size;
	  tree t = build_component_ref (array_ref, get_identifier ("__t"));
	  tree forxx = build_component_ref (t, get_identifier ("__loccharstring"));
	  tree string = build_component_ref (forxx, get_identifier ("string"));
	  tree length = build_component_ref (forxx, get_identifier ("string_length"));

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read)
	    {
	      /* in this read case the argument must be referable */
	      if (! CH_REFERABLE (item))
		{
		  error ("argument %d must be referable", idxcnt + 1 + argoffset);
		  continue;
		}
	      else
		item_addr = force_addr_of (item);
	      the_size = size_in_bytes (item_type);
	      enumname = "__IO_CharStrLoc";
	    }
	  else
	    {
	      if (! CH_REFERABLE (item))
		{
		  /* in the write case we create a temporary if not referable */
		  rtx t;
		  int howmuchbytes;

		  howmuchbytes = int_size_in_bytes (item_type);
		  if (howmuchbytes != -1)
		    {
		      /* fixed size */
		      tree loc = build_decl (VAR_DECL,
					     get_unique_identifier ("WRTEXTVS"),
					     item_type);
		      t = assign_temp (item_type, 0, 1, 0);
		      DECL_RTL (loc) = t;
		      expand_assignment (loc, item, 0, 0);
		      item_addr = force_addr_of (loc);
		      the_size = size_in_bytes (item_type);
		      enumname = "__IO_CharStrLoc";
		    }
		  else
		    {
		      tree type, string, exp, loc;

		      if ((howmuchbytes = intsize_of_charsexpr (item)) == -1)
			{
			  error ("cannot process argument %d of WRITETEXT, unknown size",
				 idxcnt + 1 + argoffset);
			  continue;
			}
		      string = build_string_type (char_type_node,
						  build_int_2 (howmuchbytes, 0));
		      type = build_varying_struct (string);
		      loc = build_decl (VAR_DECL,
					get_unique_identifier ("WRTEXTCS"),
					type);
		      t = assign_temp (type, 0, 1, 0);
		      DECL_RTL (loc) = t;
		      exp = chill_convert_for_assignment (type, item, 0);
		      expand_assignment (loc, exp, 0, 0);
		      item_addr = force_addr_of (loc);
		      the_size = integer_zero_node;
		      enumname = "__IO_CharVaryingLoc";
		    }
		}
	      else
		{
		  item_addr = force_addr_of (item);
		  the_size = size_in_bytes (item_type);
		  enumname = "__IO_CharStrLoc";
		}
	    }

	  expand_assignment (string, item_addr, 0, 0);
	  expand_assignment (length, size_in_bytes (item_type), 0, 0);

	}
      else if (CH_BOOLS_TYPE_P (item_type))
	{
	  /* we have a bitstring */
	  tree t = build_component_ref (array_ref, get_identifier ("__t"));
	  tree forxx = build_component_ref (t, get_identifier ("__loccharstring"));
	  tree string = build_component_ref (forxx, get_identifier ("string"));
	  tree length = build_component_ref (forxx, get_identifier ("string_length"));

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read)
	    {
	      /* in this read case the argument must be referable */
	      if (! referable)
		{
		  error ("argument %d must be referable", idxcnt + 1 + argoffset);
		  continue;
		}
	    }
	  else if (! referable)
	    {
	      /* in the write case we create a temporary if not referable */
	      tree loc = build_decl (VAR_DECL,
				     get_unique_identifier ("WRTEXTVS"),
				     item_type);
	      DECL_RTL (loc) = assign_temp (item_type, 0, 1, 0);
	      expand_assignment (loc, item, 0, 0);
	      item_addr = force_addr_of (loc);
	    }

	  expand_assignment (string, item_addr, 0, 0);
	  expand_assignment (length, build_chill_length (item), 0, 0);

	  enumname = "__IO_BitStrLoc";
	}
      else if (TREE_CODE (item_type) == REAL_TYPE)
	{
	  /* process a (long_)real */
	  tree	t, forxx, to_assign;

	  if (do_read && readonly)
	    {
	      error ("argument %d is READonly", idxcnt + 1 + argoffset);
	      continue;
	    }
	  if (do_read && ! referable)
	    {
	      error ("argument %d must be referable", idxcnt + 1 + argoffset);
	      continue;
	    }

	  if (lookup_name (ridpointers[RID_FLOAT]) == TYPE_NAME (item_type))
	    {
	      /* we have a real */
	      if (do_read)
		{
		  enumname = "__IO_RealLoc";
		  fieldname = "__locreal";
		  to_assign = item_addr;
		}
	      else
		{
		  enumname = "__IO_RealVal";
		  fieldname = "__valreal";
		  to_assign = item;
		}
	    }
	  else
	    {
	      /* we have a long_real */
	      if (do_read)
		{
		  enumname = "__IO_LongRealLoc";
		  fieldname = "__loclongreal";
		  to_assign = item_addr;
		}
	      else
		{
		  enumname = "__IO_LongRealVal";
		  fieldname = "__vallongreal";
		  to_assign = item;
		}
	    }
	  t = build_component_ref (array_ref, get_identifier ("__t"));
	  forxx = build_component_ref (t, get_identifier (fieldname));
	  expand_assignment (forxx, to_assign, 0, 0);
	}
#if 0
      /* don't process them for now */
      else if (TREE_CODE (item_type) == POINTER_TYPE)
	{
	  /* we have a pointer */
	  tree	__t, __forxx;
	      
	  __t = build_component_ref (array_ref, get_identifier ("__t"));
	  __forxx = build_component_ref (__t, get_identifier ("__forpointer"));
	  expand_assignment (__forxx, item, 0, 0);
	  enumname = "_IO_Pointer";
	}
      else if (item_type == instance_type_node)
	{
	  /* we have an INSTANCE */
	  tree	__t, __forxx;
	      
	  __t = build_component_ref (array_ref, get_identifier ("__t"));
	  __forxx = build_component_ref (__t, get_identifier ("__forinstance"));
	  expand_assignment (__forxx, item, 0, 0);
	  enumname = "_IO_Instance";
	}
#endif
      else
	{
	  /* datatype is not yet implemented, issue a warning */
	  error ("cannot process mode of argument %d for %sTEXT.", idxcnt + 1 + argoffset,
		 do_read ? "READ" : "WRITE");
	  enumname = "__IO_UNUSED";
	}
	  
      /* do assignment of the enum */
      if (enumname)
	{
	  tree descr = build_component_ref (array_ref,
					    get_identifier ("__descr"));
	  expand_assignment (descr,
			     lookup_name (get_identifier (enumname)), 0, 0);
	}
    }
  
  /* set up address and length of iolist */
  *iolist_addr = build_chill_addr_expr (iolist, (char *)0);
  *iolist_length = build_int_2 (iolen, 0);
}

/* check the format string */
#define LET 0x0001
#define BIN 0x0002
#define DEC 0x0004
#define OCT 0x0008
#define HEX 0x0010
#define USC 0x0020
#define BIL 0x0040
#define SPC 0x0080
#define SCS 0x0100
#define IOC 0x0200
#define EDC 0x0400
#define CVC 0x0800

#define isDEC(c)  ( chartab[(c)] & DEC )
#define isCVC(c)  ( chartab[(c)] & CVC )
#define isEDC(c)  ( chartab[(c)] & EDC )
#define isIOC(c)  ( chartab[(c)] & IOC )
#define isUSC(c)
#define isXXX(c,XXX)  ( chartab[(c)] & XXX )

static
short int chartab[256] = {
  0, 0, 0, 0, 0, 0, 0, 0, 
  0, SPC, SPC, SPC, SPC, SPC, 0, 0, 

  0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 

  SPC, IOC, 0, 0, 0, 0, 0, 0, 
  SCS, SCS, SCS, SCS+IOC, SCS, SCS+IOC, SCS, SCS+IOC, 
  BIN+OCT+DEC+HEX, BIN+OCT+DEC+HEX, OCT+DEC+HEX, OCT+DEC+HEX, OCT+DEC+HEX,
     OCT+DEC+HEX, OCT+DEC+HEX, OCT+DEC+HEX, 
  DEC+HEX, DEC+HEX, SCS, SCS, SCS+EDC, SCS+IOC, SCS+EDC, IOC, 

  0, LET+HEX+BIL, LET+HEX+BIL+CVC, LET+HEX+BIL+CVC, LET+HEX+BIL, LET+HEX, 
     LET+HEX+CVC, LET, 
  LET+BIL+CVC, LET, LET, LET, LET, LET, LET, LET+CVC, 

  LET, LET, LET, LET, LET+EDC, LET, LET, LET,
  LET+EDC, LET, LET, SCS, 0, SCS, 0, USC, 

  0, LET+HEX, LET+HEX, LET+HEX, LET+HEX, LET+HEX, LET+HEX, LET, 
  LET, LET, LET, LET, LET, LET, LET, LET, 

  LET, LET, LET, LET, LET, LET, LET, LET,
  LET, LET, LET, 0, 0, 0, 0, 0 
};

typedef enum
{
  FormatText, FirstPercent, RepFact, ConvClause, EditClause, ClauseEnd,
  AfterWidth, FractWidth, FractWidthCont, ExpoWidth, ExpoWidthCont, 
  ClauseWidth, CatchPadding, LastPercent
} fcsstate_t;

#define CONVERSIONCODES "CHOBF"
typedef enum
{
  DefaultConv, HexConv, OctalConv, BinaryConv, ScientConv
} convcode_t;
static convcode_t     convcode;

static tree check_exprlist		PARAMS ((convcode_t, tree, int,
						unsigned long));

typedef enum
{
  False, True,
} Boolean;

static unsigned long  fractionwidth;

#define IOCODES "/+-?!="
typedef enum {
  NextRecord, NextPage, CurrentLine, Prompt, Emit, EndPage
} iocode_t;
static iocode_t       iocode;

#define EDITCODES "X<>T"
typedef enum {
  SpaceSkip, SkipLeft, SkipRight, Tabulation
} editcode_t;
static editcode_t     editcode;

static unsigned long  clausewidth;
static Boolean        leftadjust;
static Boolean        overflowev;
static Boolean        dynamicwid;
static Boolean        paddingdef;
static char           paddingchar;
static Boolean        fractiondef;
static Boolean        exponentdef;
static unsigned long  exponentwidth;
static unsigned long  repetition;

typedef enum {
  NormalEnd, EndAtParen, TextFailEnd 
} formatexit_t;

static formatexit_t scanformcont	PARAMS ((char *, int, char **, int *,
						tree, tree *, int, int *));

/* NOTE: varibale have to be set to False before calling check_format_string */
static Boolean empty_printed;

static int formstroffset;

static tree
check_exprlist (code, exprlist, argnum, repetition)
     convcode_t code;
     tree exprlist;
     int argnum;
     unsigned long repetition;
{
  tree expr, type, result = NULL_TREE;

  while (repetition--)
    {
      if (exprlist == NULL_TREE)
	{
	  if (empty_printed == False)
	    {
	      warning ("too few arguments for this format string");
	      empty_printed = True;
	    }
	  return NULL_TREE;
	}
      expr = TREE_VALUE (exprlist);
      result = exprlist = TREE_CHAIN (exprlist);
      if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
	return result;
      type = TREE_TYPE (expr);
      if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
	return result;
      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
      if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
	return result;
      
      switch (code)
	{
	case DefaultConv:
	  /* %C, everything is allowed. Not know types are flaged later. */
	  break;
	case ScientConv:
	  /* %F, must be a REAL */
	  if (TREE_CODE (type) != REAL_TYPE)
	    warning ("type of argument %d invalid for conversion code at offset %d",
		     argnum, formstroffset);
	  break;
	case HexConv:
	case OctalConv:
	case BinaryConv:
	case -1:
	  /* %H, %O, %B, and V as clause width */
	  if (TREE_CODE (type) != INTEGER_TYPE)
	    warning ("type of argument %d invalid for conversion code at offset %d",
		     argnum, formstroffset);
	  break;
	default:
	  /* there is an invalid conversion code */
	  break;
	}
    }
  return result;
}

static formatexit_t
scanformcont (fcs, len, fcsptr, lenptr, exprlist, exprptr,
	      firstargnum, nextargnum)
     char *fcs;
     int len;
     char **fcsptr;
     int *lenptr;
     tree exprlist;
     tree *exprptr;
     int firstargnum;
     int *nextargnum;
{
  fcsstate_t state = FormatText;
  unsigned char curr;
  int dig;

  while (len--)
    {
      curr = *fcs++;
      formstroffset++;
      switch (state)
	{
	case FormatText: 
	  if (curr == '%')
	    state = FirstPercent;
	  break;
	  
	after_first_percent: ;
	case FirstPercent: 
	  if (curr == '%')
	    {
	      state = FormatText;
	      break;
	    }
	  if (curr == ')')
	    {
	      *lenptr = len;
	      *fcsptr = fcs;
	      *exprptr = exprlist;
	      *nextargnum = firstargnum;
	      return EndAtParen;
	    }
	  if (isDEC (curr))
	    {
	      state = RepFact;
	      repetition = curr - '0';
	      break;
	    }
	  
	  repetition = 1; 
	  
	test_for_control_codes: ;
	  if (isCVC (curr))
	    {
	      state = ConvClause;
	      convcode = strchr (CONVERSIONCODES, curr) - CONVERSIONCODES;
	      leftadjust = False;
	      overflowev = False;
	      dynamicwid = False;
	      paddingdef = False;
	      paddingchar = ' ';
	      fractiondef = False;
	      /* fractionwidth = 0; default depends on mode ! */
	      exponentdef = False;
	      exponentwidth = 3;
	      clausewidth = 0;
	      /* check the argument */
	      exprlist = check_exprlist (convcode, exprlist, firstargnum, repetition);
	      firstargnum++;
	      break;        
	    }
	  if (isEDC (curr))
	    {
	      state = EditClause;
	      editcode = strchr (EDITCODES, curr) - EDITCODES;
	      dynamicwid = False;
	      clausewidth = editcode == Tabulation ? 0 : 1;        
	      break;        
	    }
	  if (isIOC (curr))
	    {
	      state = ClauseEnd;
	      iocode = strchr (IOCODES, curr) - IOCODES;
	      break;        
	    }
	  if (curr == '(')
	    {
	      unsigned long times = repetition;
	      int  cntlen;
	      char* cntfcs;
	      tree cntexprlist;
	      int nextarg;

	      while (times--)
		{
		  if (scanformcont (fcs, len, &cntfcs, &cntlen,
				    exprlist, &cntexprlist,
				    firstargnum, &nextarg) != EndAtParen )
		    {
		      warning ("unmatched open paren");
		      break;
		    }
		  exprlist = cntexprlist;
		}
	      fcs = cntfcs;
	      len = cntlen;
	      if (len < 0)
		len = 0;
	      exprlist = cntexprlist;
	      firstargnum = nextarg;
	      state  = FormatText;
	      break;
	    }
	  warning ("bad format specification character (offset %d)", formstroffset);
	  state = FormatText;
	  /* skip one argument */
	  if (exprlist != NULL_TREE)
	    exprlist = TREE_CHAIN (exprlist);
	  break;
	  
	case RepFact:
	  if (isDEC (curr))
	    {
	      dig = curr - '0';
	      if (repetition > (ULONG_MAX - dig)/10)
		{
		  warning ("repetition factor overflow (offset %d)", formstroffset);
		  return TextFailEnd;
		}
	      repetition = repetition*10 + dig;
	      break;
	    }
	  goto test_for_control_codes;
	  
	case ConvClause:
	  if (isDEC (curr))
	    {
	      state = ClauseWidth;
	      clausewidth = curr - '0';
	      break;
	    }
	  if (curr == 'L')  
	    {
	      if (leftadjust)
		warning ("duplicate qualifier (offset %d)", formstroffset);
	      leftadjust = True;
	      break;
	    }
	  if (curr == 'E')
	    {
	      if (overflowev)
		warning ("duplicate qualifier (offset %d)", formstroffset);
	      overflowev = True;
	      break;
	    }
	  if (curr == 'P')
	    {
	      if (paddingdef)
		warning ("duplicate qualifier (offset %d)", formstroffset);
	      paddingdef = True;
	      state = CatchPadding;
	      break;
	    }
	  
	test_for_variable_width: ;
	  if (curr == 'V')
	    {
	      dynamicwid = True;
	      state = AfterWidth;
	      exprlist = check_exprlist (-1, exprlist, firstargnum, 1);
	      firstargnum++;
	      break;
	    }
	  goto test_for_fraction_width;
	  
	case ClauseWidth:
	  if (isDEC (curr))
	    {
	      dig = curr - '0';
	      if (clausewidth > (ULONG_MAX - dig)/10)
		warning ("clause width overflow (offset %d)", formstroffset);
	      else
		clausewidth = clausewidth*10 + dig;
	      break;
	    }
	  /* fall through */
	  
	test_for_fraction_width: ;
	case AfterWidth:
	  if (curr == '.')
	    {
	      if (convcode != DefaultConv && convcode != ScientConv)
		{
		  warning ("no fraction (offset %d)", formstroffset);
		  state = FormatText;
		  break;
		}
	      fractiondef = True;
	      state = FractWidth;
	      break;
	    }
	  goto test_for_exponent_width;
	  
	case FractWidth:
	  if (isDEC (curr))
	    {
	      state = FractWidthCont;
	      fractionwidth = curr - '0';
	      break;
	    }
	  else
	    warning ("no fraction width (offset %d)", formstroffset);
	  
	case FractWidthCont:
	  if (isDEC (curr))
	    {
	      dig = curr - '0';
	      if (fractionwidth > (ULONG_MAX - dig)/10)
		warning ("fraction width overflow (offset %d)", formstroffset);
	      else
		fractionwidth = fractionwidth*10 + dig;
	      break;
	    }
	  
	test_for_exponent_width: ;
	  if (curr == ':')
	    {
	      if (convcode != ScientConv)
		{
		  warning ("no exponent (offset %d)", formstroffset);
		  state = FormatText;
		  break;
		}
	      exponentdef = True;
	      state = ExpoWidth;
	      break;
	    }
	  goto test_for_final_percent;
	  
	case ExpoWidth:
	  if (isDEC (curr))
	    {
	      state = ExpoWidthCont;
	      exponentwidth = curr - '0';
	      break;
	    }
	  else
	    warning ("no exponent width (offset %d)", formstroffset);
	  
	case ExpoWidthCont:
	  if (isDEC (curr))
	    {
	      dig = curr - '0';
	      if (exponentwidth > (ULONG_MAX - dig)/10)
		warning ("exponent width overflow (offset %d)", formstroffset);
	      else
		exponentwidth = exponentwidth*10 + dig;
	      break;
	    }
	  /* fall through  */
	  
	test_for_final_percent: ;
	case ClauseEnd:
	  if (curr == '%')
	    {
	      state = LastPercent;
	      break;
	    }
	  
	  state = FormatText;
	  break;
	  
	case CatchPadding:
	  paddingchar = curr;
	  state = ConvClause;
	  break;
	  
	case EditClause:
	  if (isDEC (curr))
	    {
	      state = ClauseWidth;
	      clausewidth = curr - '0';
	      break;
	    }
	  goto test_for_variable_width; 
	  
	case LastPercent:
	  if (curr == '.')
	    {
	      state = FormatText;
	      break;
	    }
	  goto after_first_percent;
	  
	default:
	  error ("internal error in check_format_string");
	}
    }

  switch (state)
    {
    case FormatText:
      break;
    case FirstPercent:
    case LastPercent:
    case RepFact:
    case FractWidth:
    case ExpoWidth:
      warning ("bad format specification character (offset %d)", formstroffset);      
      break;
    case CatchPadding:
      warning ("no padding character (offset %d)", formstroffset);
      break;
    default:
      break;
    }
  *fcsptr = fcs;
  *lenptr = len;
  *exprptr = exprlist;
  *nextargnum = firstargnum;
  return NormalEnd;
}
static void
check_format_string (format_str, exprlist, firstargnum)
     tree format_str;
     tree exprlist;
     int firstargnum;
{
  char *x;
  int y, yy;
  tree z = NULL_TREE;

  if (TREE_CODE (format_str) != STRING_CST)
    /* do nothing if we don't have a string constant */
    return;

  formstroffset = -1;
  scanformcont (TREE_STRING_POINTER (format_str),
		TREE_STRING_LENGTH (format_str), &x, &y,
		exprlist, &z,
		firstargnum, &yy);
  if (z != NULL_TREE)
    /* too  may arguments for format string */
    warning ("too many arguments for this format string");
}

static int
get_max_size (expr)
     tree expr;
{
  if (TREE_CODE (expr) == INDIRECT_REF)
    {
      tree x = TREE_OPERAND (expr, 0);
      tree y = TREE_OPERAND (x, 0);
      return int_size_in_bytes (TREE_TYPE (y));
    }
  else if (TREE_CODE (expr) == CONCAT_EXPR)
    return intsize_of_charsexpr (expr);
  else
    return int_size_in_bytes (TREE_TYPE (expr));
}

static int
intsize_of_charsexpr (expr)
     tree expr;
{
  int op0size, op1size;

  if (TREE_CODE (expr) != CONCAT_EXPR)
    return -1;

  /* find maximum length of CONCAT_EXPR, this is the worst case */
  op0size = get_max_size (TREE_OPERAND (expr, 0));
  op1size = get_max_size (TREE_OPERAND (expr, 1));
  if (op0size == -1 || op1size == -1)
    return -1;
  return op0size + op1size;
}

tree
build_chill_writetext (text_arg, exprlist)
     tree text_arg, exprlist;
{
  tree iolist_addr = null_pointer_node;
  tree iolist_length = integer_zero_node;
  tree fstr_addr;
  tree fstr_length;
  tree outstr_addr;
  tree outstr_length;
  tree fstrtype;
  tree outfunction;
  tree filename, linenumber;
  tree format_str = NULL_TREE, indexexpr = NULL_TREE;
  rtx  iolist_rtx = NULL_RTX;
  int argoffset = 0;

  /* make some checks */
  if (text_arg == NULL_TREE || TREE_CODE (text_arg) == ERROR_MARK)
    return error_mark_node;

  if (exprlist != NULL_TREE)
    {
      if (TREE_CODE (exprlist) != TREE_LIST)
	return error_mark_node;
    }
  
  /* check the text argument */
  if (chill_varying_string_type_p (TREE_TYPE (text_arg)))
    {
      /* build outstr-addr and outstr-length assuming that this is a CHAR (n) VARYING */
      outstr_addr = force_addr_of (text_arg);
      outstr_length = size_in_bytes (CH_VARYING_ARRAY_TYPE (TREE_TYPE (text_arg)));
      outfunction = lookup_name (get_identifier ("__writetext_s"));
      format_str = TREE_VALUE (exprlist);
      exprlist = TREE_CHAIN (exprlist);
    }
  else if (CH_IS_TEXT_MODE (TREE_TYPE (text_arg)))
    {
      /* we have a text mode */
      tree indexmode;

      if (! check_text (text_arg, 1, "WRITETEXT"))
	return error_mark_node;
      indexmode = text_indexmode (TREE_TYPE (text_arg));
      if (indexmode == void_type_node)
	{
	  /* no index */
	  format_str = TREE_VALUE (exprlist);
	  exprlist = TREE_CHAIN (exprlist);
	}
      else
	{
	  /* we have an index. there must be an index argument before format string */
	  indexexpr = TREE_VALUE (exprlist);
	  exprlist = TREE_CHAIN (exprlist);
	  if (! CH_COMPATIBLE (indexexpr, indexmode))
	    {
	      if (chill_varying_string_type_p (TREE_TYPE (indexexpr)) ||
		  (CH_CHARS_TYPE_P (TREE_TYPE (indexexpr)) ||
		   (flag_old_strings && TREE_CODE (indexexpr) == INTEGER_CST &&
		    TREE_CODE (TREE_TYPE (indexexpr)) == CHAR_TYPE)))
		error ("missing index expression");
	      else
		error ("incompatible index mode");
	      return error_mark_node;
	    }
	  if (exprlist == NULL_TREE)
	    {
	      error ("Too few arguments in call to `writetext'");
	      return error_mark_node;
	    }
	  format_str = TREE_VALUE (exprlist);
	  exprlist = TREE_CHAIN (exprlist);
	  argoffset = 1;
	}
      outstr_addr = force_addr_of (text_arg);
      outstr_length = convert (integer_type_node, indexexpr);
      outfunction = lookup_name (get_identifier ("__writetext_f"));
    }
  else
    {
      error ("argument 1 for WRITETEXT must be a TEXT or CHARS(n) VARYING location");
      return error_mark_node;
    }
  
  /* check the format string */
  fstrtype = TREE_TYPE (format_str);
  if (CH_CHARS_TYPE_P (fstrtype) ||
      (flag_old_strings && TREE_CODE (format_str) == INTEGER_CST &&
       TREE_CODE (fstrtype) == CHAR_TYPE))
    {
      /* we have a character string */
      fstr_addr = force_addr_of (format_str);
      fstr_length = size_in_bytes (fstrtype);
    }
  else if (chill_varying_string_type_p (TREE_TYPE (format_str)))
    {
      /* we have a varying char string */
      fstr_addr
	= force_addr_of (build_component_ref (format_str, var_data_id));
      fstr_length = build_component_ref (format_str, var_length_id);
    }
  else
    {
      error ("`format string' for WRITETEXT must be a CHARACTER string");
      return error_mark_node;
    }

  empty_printed = False;
  check_format_string (format_str, exprlist, argoffset + 3);
  process_io_list (exprlist, &iolist_addr, &iolist_length, &iolist_rtx, 0, argoffset);
  
  /* tree to call the function */

  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();

  expand_expr_stmt (
    build_chill_function_call (outfunction,
      tree_cons (NULL_TREE, outstr_addr,
	tree_cons (NULL_TREE, outstr_length,
	  tree_cons (NULL_TREE, fstr_addr,
	    tree_cons (NULL_TREE, fstr_length,
	      tree_cons (NULL_TREE, iolist_addr,
		tree_cons (NULL_TREE, iolist_length,
		  tree_cons (NULL_TREE, filename,
		    tree_cons (NULL_TREE, linenumber,
		      NULL_TREE))))))))));

  /* get rid of the iolist variable, if we have one */
  if (iolist_rtx != NULL_RTX)
    {
      free_temp_slots ();
      pop_temp_slots ();
      free_temp_slots ();
      pop_temp_slots ();
    }

  /* return something the rest of the machinery can work with,
     i.e. (void)0 */
  return build1 (CONVERT_EXPR, void_type_node, integer_zero_node);
}

tree
build_chill_readtext (text_arg, exprlist)
     tree text_arg, exprlist;
{
  tree instr_addr, instr_length, infunction;
  tree fstr_addr, fstr_length, fstrtype;
  tree iolist_addr = null_pointer_node;
  tree iolist_length = integer_zero_node;
  tree filename, linenumber;
  tree format_str = NULL_TREE, indexexpr = NULL_TREE;
  rtx  iolist_rtx = NULL_RTX;
  int argoffset = 0;

  /* make some checks */
  if (text_arg == NULL_TREE || TREE_CODE (text_arg) == ERROR_MARK)
    return error_mark_node;

  if (exprlist != NULL_TREE)
    {
      if (TREE_CODE (exprlist) != TREE_LIST)
	return error_mark_node;
    }
  
  /* check the text argument */
  if (CH_CHARS_TYPE_P (TREE_TYPE (text_arg)))
    {
      instr_addr = force_addr_of (text_arg);
      instr_length = size_in_bytes (TREE_TYPE (text_arg));
      infunction = lookup_name (get_identifier ("__readtext_s"));
      format_str = TREE_VALUE (exprlist);
      exprlist = TREE_CHAIN (exprlist);
    }
  else if (chill_varying_string_type_p (TREE_TYPE (text_arg)))
    {
      instr_addr
	= force_addr_of (build_component_ref (text_arg, var_data_id));
      instr_length = build_component_ref (text_arg, var_length_id);
      infunction = lookup_name (get_identifier ("__readtext_s"));
      format_str = TREE_VALUE (exprlist);
      exprlist = TREE_CHAIN (exprlist);
    }
  else if (CH_IS_TEXT_MODE (TREE_TYPE (text_arg)))
    {
      /* we have a text mode */
      tree indexmode;

      if (! check_text (text_arg, 1, "READTEXT"))
	return error_mark_node;
      indexmode = text_indexmode (TREE_TYPE (text_arg));
      if (indexmode == void_type_node)
	{
	  /* no index */
	  format_str = TREE_VALUE (exprlist);
	  exprlist = TREE_CHAIN (exprlist);
	}
      else
	{
	  /* we have an index. there must be an index argument before format string */
	  indexexpr = TREE_VALUE (exprlist);
	  exprlist = TREE_CHAIN (exprlist);
	  if (! CH_COMPATIBLE (indexexpr, indexmode))
	    {
	      if (chill_varying_string_type_p (TREE_TYPE (indexexpr)) ||
		  (CH_CHARS_TYPE_P (TREE_TYPE (indexexpr)) ||
		   (flag_old_strings && TREE_CODE (indexexpr) == INTEGER_CST &&
		    TREE_CODE (TREE_TYPE (indexexpr)) == CHAR_TYPE)))
		error ("missing index expression");
	      else
		error ("incompatible index mode");
	      return error_mark_node;
	    }
	  if (exprlist == NULL_TREE)
	    {
	      error ("Too few arguments in call to `readtext'");
	      return error_mark_node;
	    }
	  format_str = TREE_VALUE (exprlist);
	  exprlist = TREE_CHAIN (exprlist);
	  argoffset = 1;
	}
      instr_addr = force_addr_of (text_arg);
      instr_length = convert (integer_type_node, indexexpr);
      infunction = lookup_name (get_identifier ("__readtext_f"));
    }
  else
    {
      error ("argument 1 for READTEXT must be a TEXT location or CHARS(n) [ VARYING ] expression");
      return error_mark_node;
    }
  
  /* check the format string */
  fstrtype = TREE_TYPE (format_str);
  if (CH_CHARS_TYPE_P (fstrtype))
    {
      /* we have a character string */
      fstr_addr = force_addr_of (format_str);
      fstr_length = size_in_bytes (fstrtype);
    }
  else if (chill_varying_string_type_p (fstrtype))
    {
      /* we have a CHARS(n) VARYING */
      fstr_addr
	= force_addr_of (build_component_ref (format_str, var_data_id));
      fstr_length = build_component_ref (format_str, var_length_id);
    }
  else
    {
      error ("`format string' for READTEXT must be a CHARACTER string");
      return error_mark_node;
    }

  empty_printed = False;
  check_format_string (format_str, exprlist, argoffset + 3);
  process_io_list (exprlist, &iolist_addr, &iolist_length, &iolist_rtx, 1, argoffset);

  /* build the function call */
  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();
  expand_expr_stmt (
    build_chill_function_call (infunction,
      tree_cons (NULL_TREE, instr_addr,
	tree_cons (NULL_TREE, instr_length,
	  tree_cons (NULL_TREE, fstr_addr,
	    tree_cons (NULL_TREE, fstr_length,
	      tree_cons (NULL_TREE, iolist_addr,
		tree_cons (NULL_TREE, iolist_length,
		  tree_cons (NULL_TREE, filename,
		    tree_cons (NULL_TREE, linenumber,
		      NULL_TREE))))))))));
  
  /* get rid of the iolist variable, if we have one */
  if (iolist_rtx != NULL_RTX)
    {
      free_temp_slots ();
      pop_temp_slots ();
      free_temp_slots ();
      pop_temp_slots ();
    }
  
  /* return something the rest of the machinery can work with,
     i.e. (void)0 */
  return build1 (CONVERT_EXPR, void_type_node, integer_zero_node);
}

/* this function build all neccesary enum-tables used for
   WRITETEXT or READTEXT of an enum */

void build_enum_tables ()
{
  SAVE_ENUM_NAMES	*names;
  SAVE_ENUMS		*wrk;
  void		*saveptr;
  /* We temporarily reset the maximum_field_alignment to zero so the
     compiler's init data structures can be compatible with the
     run-time system, even when we're compiling with -fpack. */
  unsigned int save_maximum_field_alignment;
    
  if (pass == 1)
    return;

  save_maximum_field_alignment = maximum_field_alignment;
  maximum_field_alignment = 0;

  /* output all names */
  names = used_enum_names;
    
  while (names != (SAVE_ENUM_NAMES *)0)
    {
      tree	var = get_unique_identifier ("ENUMNAME");
      tree	type;
	
      type = build_string_type (char_type_node,
				build_int_2 (IDENTIFIER_LENGTH (names->name) + 1, 0));
      names->decl = decl_temp1 (var, type, 1,
				build_chill_string (IDENTIFIER_LENGTH (names->name) + 1,
						    IDENTIFIER_POINTER (names->name)),
				0, 0);
      names = names->forward;
    }

  /* output the tables and pointers to tables */
  wrk = used_enums;
  while (wrk != (SAVE_ENUMS *)0)
    {
      tree	varptr = wrk->ptrdecl;
      tree	table_addr = null_pointer_node;
      tree	init = NULL_TREE, one_entry;
      tree	table, idxlist, tabletype, addr;
      SAVE_ENUM_VALUES	*vals;
      int	i;
	
      vals = wrk->vals;
      for (i = 0; i < wrk->num_vals; i++)
	{
	  tree decl = vals->name->decl;
	  addr = build1 (ADDR_EXPR,
			 build_pointer_type (char_type_node),
			 decl);
	  TREE_CONSTANT (addr) = 1;
	  one_entry = tree_cons (NULL_TREE, build_int_2 (vals->val, 0),
				 tree_cons (NULL_TREE, addr, NULL_TREE));
	  one_entry = build_nt (CONSTRUCTOR, NULL_TREE, one_entry);
	  init = tree_cons (NULL_TREE, one_entry, init);
	  vals++;
	}

      /* add the terminator (name = null_pointer_node) to constructor */
      one_entry = tree_cons (NULL_TREE, integer_zero_node,
			     tree_cons (NULL_TREE, null_pointer_node, NULL_TREE));
      one_entry = build_nt (CONSTRUCTOR, NULL_TREE, one_entry);
      init = tree_cons (NULL_TREE, one_entry, init);
      init = nreverse (init);
      init = build_nt (CONSTRUCTOR, NULL_TREE, init);
      TREE_CONSTANT (init) = 1;

      /* generate table */
      idxlist = build_tree_list (NULL_TREE,
				 build_chill_range_type (NULL_TREE,
							 integer_zero_node,
							 build_int_2 (wrk->num_vals, 0)));
      tabletype = build_chill_array_type (TREE_TYPE (enum_table_type),
					  idxlist, 0, NULL_TREE);
      table = decl_temp1 (get_unique_identifier ("ENUMTAB"), tabletype,
			  1, init, 0, 0);
      table_addr = build1 (ADDR_EXPR,
			   build_pointer_type (TREE_TYPE (enum_table_type)),
			   table);
      TREE_CONSTANT (table_addr) = 1;

      /* generate pointer to table */
      decl_temp1 (DECL_NAME (varptr), TREE_TYPE (table_addr),
		  1, table_addr, 0, 0);

      /* free that stuff */
      saveptr = wrk->forward;
	
      free (wrk->vals);
      free (wrk);
	
      /* next enum */
      wrk = saveptr;
    }

  /* free all the names */
  names = used_enum_names;
  while (names != (SAVE_ENUM_NAMES *)0)
    {
      saveptr = names->forward;
      free (names);
      names = saveptr;
    }

  used_enums = (SAVE_ENUMS *)0;
  used_enum_names = (SAVE_ENUM_NAMES *)0;
  maximum_field_alignment = save_maximum_field_alignment;
}
