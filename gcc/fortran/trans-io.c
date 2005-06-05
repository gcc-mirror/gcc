/* IO Code translation/library interface
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-gimple.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"


/* Members of the ioparm structure.  */

static GTY(()) tree ioparm_unit;
static GTY(()) tree ioparm_err;
static GTY(()) tree ioparm_end;
static GTY(()) tree ioparm_eor;
static GTY(()) tree ioparm_list_format;
static GTY(()) tree ioparm_library_return;
static GTY(()) tree ioparm_iostat;
static GTY(()) tree ioparm_exist;
static GTY(()) tree ioparm_opened;
static GTY(()) tree ioparm_number;
static GTY(()) tree ioparm_named;
static GTY(()) tree ioparm_rec;
static GTY(()) tree ioparm_nextrec;
static GTY(()) tree ioparm_size;
static GTY(()) tree ioparm_recl_in;
static GTY(()) tree ioparm_recl_out;
static GTY(()) tree ioparm_iolength;
static GTY(()) tree ioparm_file;
static GTY(()) tree ioparm_file_len;
static GTY(()) tree ioparm_status;
static GTY(()) tree ioparm_status_len;
static GTY(()) tree ioparm_access;
static GTY(()) tree ioparm_access_len;
static GTY(()) tree ioparm_form;
static GTY(()) tree ioparm_form_len;
static GTY(()) tree ioparm_blank;
static GTY(()) tree ioparm_blank_len;
static GTY(()) tree ioparm_position;
static GTY(()) tree ioparm_position_len;
static GTY(()) tree ioparm_action;
static GTY(()) tree ioparm_action_len;
static GTY(()) tree ioparm_delim;
static GTY(()) tree ioparm_delim_len;
static GTY(()) tree ioparm_pad;
static GTY(()) tree ioparm_pad_len;
static GTY(()) tree ioparm_format;
static GTY(()) tree ioparm_format_len;
static GTY(()) tree ioparm_advance;
static GTY(()) tree ioparm_advance_len;
static GTY(()) tree ioparm_name;
static GTY(()) tree ioparm_name_len;
static GTY(()) tree ioparm_internal_unit;
static GTY(()) tree ioparm_internal_unit_len;
static GTY(()) tree ioparm_sequential;
static GTY(()) tree ioparm_sequential_len;
static GTY(()) tree ioparm_direct;
static GTY(()) tree ioparm_direct_len;
static GTY(()) tree ioparm_formatted;
static GTY(()) tree ioparm_formatted_len;
static GTY(()) tree ioparm_unformatted;
static GTY(()) tree ioparm_unformatted_len;
static GTY(()) tree ioparm_read;
static GTY(()) tree ioparm_read_len;
static GTY(()) tree ioparm_write;
static GTY(()) tree ioparm_write_len;
static GTY(()) tree ioparm_readwrite;
static GTY(()) tree ioparm_readwrite_len;
static GTY(()) tree ioparm_namelist_name;
static GTY(()) tree ioparm_namelist_name_len;
static GTY(()) tree ioparm_namelist_read_mode;

/* The global I/O variables */

static GTY(()) tree ioparm_var;
static GTY(()) tree locus_file;
static GTY(()) tree locus_line;


/* Library I/O subroutines */

static GTY(()) tree iocall_read;
static GTY(()) tree iocall_read_done;
static GTY(()) tree iocall_write;
static GTY(()) tree iocall_write_done;
static GTY(()) tree iocall_x_integer;
static GTY(()) tree iocall_x_logical;
static GTY(()) tree iocall_x_character;
static GTY(()) tree iocall_x_real;
static GTY(()) tree iocall_x_complex;
static GTY(()) tree iocall_open;
static GTY(()) tree iocall_close;
static GTY(()) tree iocall_inquire;
static GTY(()) tree iocall_iolength;
static GTY(()) tree iocall_iolength_done;
static GTY(()) tree iocall_rewind;
static GTY(()) tree iocall_backspace;
static GTY(()) tree iocall_endfile;
static GTY(()) tree iocall_set_nml_val;
static GTY(()) tree iocall_set_nml_val_dim;

/* Variable for keeping track of what the last data transfer statement
   was.  Used for deciding which subroutine to call when the data
   transfer is complete.  */
static enum { READ, WRITE, IOLENGTH } last_dt;

#define ADD_FIELD(name, type)						\
  ioparm_ ## name = gfc_add_field_to_struct				\
        (&(TYPE_FIELDS (ioparm_type)), ioparm_type,			\
	 get_identifier (stringize(name)), type)

#define ADD_STRING(name) \
  ioparm_ ## name = gfc_add_field_to_struct				\
        (&(TYPE_FIELDS (ioparm_type)), ioparm_type,			\
	 get_identifier (stringize(name)), pchar_type_node);		\
  ioparm_ ## name ## _len = gfc_add_field_to_struct			\
        (&(TYPE_FIELDS (ioparm_type)), ioparm_type,			\
	 get_identifier (stringize(name) "_len"), gfc_charlen_type_node)


/* Create function decls for IO library functions.  */

void
gfc_build_io_library_fndecls (void)
{
  tree gfc_int4_type_node;
  tree gfc_pint4_type_node;
  tree ioparm_type;

  gfc_int4_type_node = gfc_get_int_type (4);
  gfc_pint4_type_node = build_pointer_type (gfc_int4_type_node);

  /* Build the st_parameter structure.  Information associated with I/O
     calls are transferred here.  This must match the one defined in the
     library exactly.  */

  ioparm_type = make_node (RECORD_TYPE);
  TYPE_NAME (ioparm_type) = get_identifier ("_gfc_ioparm");

  ADD_FIELD (unit, gfc_int4_type_node);
  ADD_FIELD (err, gfc_int4_type_node);
  ADD_FIELD (end, gfc_int4_type_node);
  ADD_FIELD (eor, gfc_int4_type_node);
  ADD_FIELD (list_format, gfc_int4_type_node);
  ADD_FIELD (library_return, gfc_int4_type_node);

  ADD_FIELD (iostat, gfc_pint4_type_node);
  ADD_FIELD (exist, gfc_pint4_type_node);
  ADD_FIELD (opened, gfc_pint4_type_node);
  ADD_FIELD (number, gfc_pint4_type_node);
  ADD_FIELD (named, gfc_pint4_type_node);
  ADD_FIELD (rec, gfc_int4_type_node);
  ADD_FIELD (nextrec, gfc_pint4_type_node);
  ADD_FIELD (size, gfc_pint4_type_node);

  ADD_FIELD (recl_in, gfc_int4_type_node);
  ADD_FIELD (recl_out, gfc_pint4_type_node);

  ADD_FIELD (iolength, gfc_pint4_type_node);

  ADD_STRING (file);
  ADD_STRING (status);

  ADD_STRING (access);
  ADD_STRING (form);
  ADD_STRING (blank);
  ADD_STRING (position);
  ADD_STRING (action);
  ADD_STRING (delim);
  ADD_STRING (pad);
  ADD_STRING (format);
  ADD_STRING (advance);
  ADD_STRING (name);
  ADD_STRING (internal_unit);
  ADD_STRING (sequential);

  ADD_STRING (direct);
  ADD_STRING (formatted);
  ADD_STRING (unformatted);
  ADD_STRING (read);
  ADD_STRING (write);
  ADD_STRING (readwrite);

  ADD_STRING (namelist_name);
  ADD_FIELD (namelist_read_mode, gfc_int4_type_node);

  gfc_finish_type (ioparm_type);

  ioparm_var = build_decl (VAR_DECL, get_identifier (PREFIX("ioparm")),
			   ioparm_type);
  DECL_EXTERNAL (ioparm_var) = 1;
  TREE_PUBLIC (ioparm_var) = 1;

  locus_line = build_decl (VAR_DECL, get_identifier (PREFIX("line")),
			   gfc_int4_type_node);
  DECL_EXTERNAL (locus_line) = 1;
  TREE_PUBLIC (locus_line) = 1;

  locus_file = build_decl (VAR_DECL, get_identifier (PREFIX("filename")),
			   pchar_type_node);
  DECL_EXTERNAL (locus_file) = 1;
  TREE_PUBLIC (locus_file) = 1;

  /* Define the transfer functions.  */

  iocall_x_integer =
    gfc_build_library_function_decl (get_identifier
				     (PREFIX("transfer_integer")),
				     void_type_node, 2, pvoid_type_node,
				     gfc_int4_type_node);

  iocall_x_logical =
    gfc_build_library_function_decl (get_identifier
				     (PREFIX("transfer_logical")),
				     void_type_node, 2, pvoid_type_node,
				     gfc_int4_type_node);

  iocall_x_character =
    gfc_build_library_function_decl (get_identifier
				     (PREFIX("transfer_character")),
				     void_type_node, 2, pvoid_type_node,
				     gfc_int4_type_node);

  iocall_x_real =
    gfc_build_library_function_decl (get_identifier (PREFIX("transfer_real")),
				     void_type_node, 2,
				     pvoid_type_node, gfc_int4_type_node);

  iocall_x_complex =
    gfc_build_library_function_decl (get_identifier
				     (PREFIX("transfer_complex")),
				     void_type_node, 2, pvoid_type_node,
				     gfc_int4_type_node);

  /* Library entry points */

  iocall_read =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_read")),
				     void_type_node, 0);

  iocall_write =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_write")),
				     void_type_node, 0);
  iocall_open =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_open")),
				     void_type_node, 0);

  iocall_close =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_close")),
				     void_type_node, 0);

  iocall_inquire =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_inquire")),
				     gfc_int4_type_node, 0);

  iocall_iolength =
    gfc_build_library_function_decl(get_identifier (PREFIX("st_iolength")),
				    void_type_node, 0);

  iocall_rewind =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_rewind")),
				     gfc_int4_type_node, 0);

  iocall_backspace =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_backspace")),
				     gfc_int4_type_node, 0);

  iocall_endfile =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_endfile")),
				     gfc_int4_type_node, 0);
  /* Library helpers */

  iocall_read_done =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_read_done")),
				     gfc_int4_type_node, 0);

  iocall_write_done =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_write_done")),
				     gfc_int4_type_node, 0);

  iocall_iolength_done =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_iolength_done")),
				     gfc_int4_type_node, 0);


  iocall_set_nml_val =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_set_nml_var")),
                                     void_type_node, 5,
                                     pvoid_type_node, pvoid_type_node,
                                     gfc_int4_type_node, gfc_charlen_type_node, 
				     gfc_int4_type_node);

  iocall_set_nml_val_dim =
    gfc_build_library_function_decl (get_identifier (PREFIX("st_set_nml_var_dim")),
				     void_type_node, 4,
				     gfc_int4_type_node, gfc_int4_type_node,
				     gfc_int4_type_node, gfc_int4_type_node);
}


/* Generate code to store an non-string I/O parameter into the
   ioparm structure.  This is a pass by value.  */

static void
set_parameter_value (stmtblock_t * block, tree var, gfc_expr * e)
{
  gfc_se se;
  tree tmp;

  gfc_init_se (&se, NULL);
  gfc_conv_expr_type (&se, e, TREE_TYPE (var));
  gfc_add_block_to_block (block, &se.pre);

  tmp = build3 (COMPONENT_REF, TREE_TYPE (var), ioparm_var, var, NULL_TREE);
  gfc_add_modify_expr (block, tmp, se.expr);
}


/* Generate code to store an non-string I/O parameter into the
   ioparm structure.  This is pass by reference.  */

static void
set_parameter_ref (stmtblock_t * block, tree var, gfc_expr * e)
{
  gfc_se se;
  tree tmp;

  gfc_init_se (&se, NULL);
  se.want_pointer = 1;

  gfc_conv_expr_type (&se, e, TREE_TYPE (var));
  gfc_add_block_to_block (block, &se.pre);

  tmp = build3 (COMPONENT_REF, TREE_TYPE (var), ioparm_var, var, NULL_TREE);
  gfc_add_modify_expr (block, tmp, se.expr);
}


/* Generate code to store a string and its length into the
   ioparm structure.  */

static void
set_string (stmtblock_t * block, stmtblock_t * postblock, tree var,
	    tree var_len, gfc_expr * e)
{
  gfc_se se;
  tree tmp;
  tree msg;
  tree io;
  tree len;

  gfc_init_se (&se, NULL);

  io = build3 (COMPONENT_REF, TREE_TYPE (var), ioparm_var, var, NULL_TREE);
  len = build3 (COMPONENT_REF, TREE_TYPE (var_len), ioparm_var, var_len,
		NULL_TREE);

  /* Integer variable assigned a format label.  */
  if (e->ts.type == BT_INTEGER && e->symtree->n.sym->attr.assign == 1)
    {
      gfc_conv_label_variable (&se, e);
      msg =
        gfc_build_cstring_const ("Assigned label is not a format label");
      tmp = GFC_DECL_STRING_LEN (se.expr);
      tmp = build2 (LE_EXPR, boolean_type_node,
		    tmp, convert (TREE_TYPE (tmp), integer_minus_one_node));
      gfc_trans_runtime_check (tmp, msg, &se.pre);
      gfc_add_modify_expr (&se.pre, io,
		 fold_convert (TREE_TYPE (io), GFC_DECL_ASSIGN_ADDR (se.expr)));
      gfc_add_modify_expr (&se.pre, len, GFC_DECL_STRING_LEN (se.expr));
    }
  else
    {
      gfc_conv_expr (&se, e);
      gfc_conv_string_parameter (&se);
      gfc_add_modify_expr (&se.pre, io, fold_convert (TREE_TYPE (io), se.expr));
      gfc_add_modify_expr (&se.pre, len, se.string_length);
    }

  gfc_add_block_to_block (block, &se.pre);
  gfc_add_block_to_block (postblock, &se.post);

}


/* Set a member of the ioparm structure to one.  */
static void
set_flag (stmtblock_t *block, tree var)
{
  tree tmp, type = TREE_TYPE (var);

  tmp = build3 (COMPONENT_REF, type, ioparm_var, var, NULL_TREE);
  gfc_add_modify_expr (block, tmp, convert (type, integer_one_node));
}


/* Add a case to a IO-result switch.  */

static void
add_case (int label_value, gfc_st_label * label, stmtblock_t * body)
{
  tree tmp, value;

  if (label == NULL)
    return;			/* No label, no case */

  value = build_int_cst (NULL_TREE, label_value);

  /* Make a backend label for this case.  */
  tmp = gfc_build_label_decl (NULL_TREE);

  /* And the case itself.  */
  tmp = build3_v (CASE_LABEL_EXPR, value, NULL_TREE, tmp);
  gfc_add_expr_to_block (body, tmp);

  /* Jump to the label.  */
  tmp = build1_v (GOTO_EXPR, gfc_get_label_decl (label));
  gfc_add_expr_to_block (body, tmp);
}


/* Generate a switch statement that branches to the correct I/O
   result label.  The last statement of an I/O call stores the
   result into a variable because there is often cleanup that
   must be done before the switch, so a temporary would have to
   be created anyway.  */

static void
io_result (stmtblock_t * block, gfc_st_label * err_label,
	   gfc_st_label * end_label, gfc_st_label * eor_label)
{
  stmtblock_t body;
  tree tmp, rc;

  /* If no labels are specified, ignore the result instead
     of building an empty switch.  */
  if (err_label == NULL
      && end_label == NULL
      && eor_label == NULL)
    return;

  /* Build a switch statement.  */
  gfc_start_block (&body);

  /* The label values here must be the same as the values
     in the library_return enum in the runtime library */
  add_case (1, err_label, &body);
  add_case (2, end_label, &body);
  add_case (3, eor_label, &body);

  tmp = gfc_finish_block (&body);

  rc = build3 (COMPONENT_REF, TREE_TYPE (ioparm_library_return), ioparm_var,
	       ioparm_library_return, NULL_TREE);

  tmp = build3_v (SWITCH_EXPR, rc, tmp, NULL_TREE);

  gfc_add_expr_to_block (block, tmp);
}


/* Store the current file and line number to variables so that if a
   library call goes awry, we can tell the user where the problem is.  */

static void
set_error_locus (stmtblock_t * block, locus * where)
{
  gfc_file *f;
  tree tmp;
  int line;

  f = where->lb->file;
  tmp = gfc_build_cstring_const (f->filename);

  tmp = gfc_build_addr_expr (pchar_type_node, tmp);
  gfc_add_modify_expr (block, locus_file, tmp);

#ifdef USE_MAPPED_LOCATION
  line = LOCATION_LINE (where->lb->location);
#else
  line = where->lb->linenum;
#endif
  gfc_add_modify_expr (block, locus_line, build_int_cst (NULL_TREE, line));
}


/* Translate an OPEN statement.  */

tree
gfc_trans_open (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_open *p;
  tree tmp;

  gfc_init_block (&block);
  gfc_init_block (&post_block);

  set_error_locus (&block, &code->loc);
  p = code->ext.open;

  if (p->unit)
    set_parameter_value (&block, ioparm_unit, p->unit);

  if (p->file)
    set_string (&block, &post_block, ioparm_file, ioparm_file_len, p->file);

  if (p->status)
    set_string (&block, &post_block, ioparm_status,
		ioparm_status_len, p->status);

  if (p->access)
    set_string (&block, &post_block, ioparm_access,
		ioparm_access_len, p->access);

  if (p->form)
    set_string (&block, &post_block, ioparm_form, ioparm_form_len, p->form);

  if (p->recl)
    set_parameter_value (&block, ioparm_recl_in, p->recl);

  if (p->blank)
    set_string (&block, &post_block, ioparm_blank, ioparm_blank_len,
		p->blank);

  if (p->position)
    set_string (&block, &post_block, ioparm_position,
		ioparm_position_len, p->position);

  if (p->action)
    set_string (&block, &post_block, ioparm_action,
		ioparm_action_len, p->action);

  if (p->delim)
    set_string (&block, &post_block, ioparm_delim, ioparm_delim_len,
		p->delim);

  if (p->pad)
    set_string (&block, &post_block, ioparm_pad, ioparm_pad_len, p->pad);

  if (p->iostat)
    set_parameter_ref (&block, ioparm_iostat, p->iostat);

  if (p->err)
    set_flag (&block, ioparm_err);

  tmp = gfc_build_function_call (iocall_open, NULL_TREE);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


/* Translate a CLOSE statement.  */

tree
gfc_trans_close (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_close *p;
  tree tmp;

  gfc_init_block (&block);
  gfc_init_block (&post_block);

  set_error_locus (&block, &code->loc);
  p = code->ext.close;

  if (p->unit)
    set_parameter_value (&block, ioparm_unit, p->unit);

  if (p->status)
    set_string (&block, &post_block, ioparm_status,
		ioparm_status_len, p->status);

  if (p->iostat)
    set_parameter_ref (&block, ioparm_iostat, p->iostat);

  if (p->err)
    set_flag (&block, ioparm_err);

  tmp = gfc_build_function_call (iocall_close, NULL_TREE);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


/* Common subroutine for building a file positioning statement.  */

static tree
build_filepos (tree function, gfc_code * code)
{
  stmtblock_t block;
  gfc_filepos *p;
  tree tmp;

  p = code->ext.filepos;

  gfc_init_block (&block);

  set_error_locus (&block, &code->loc);

  if (p->unit)
    set_parameter_value (&block, ioparm_unit, p->unit);

  if (p->iostat)
    set_parameter_ref (&block, ioparm_iostat, p->iostat);

  if (p->err)
    set_flag (&block, ioparm_err);

  tmp = gfc_build_function_call (function, NULL);
  gfc_add_expr_to_block (&block, tmp);

  io_result (&block, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


/* Translate a BACKSPACE statement.  */

tree
gfc_trans_backspace (gfc_code * code)
{

  return build_filepos (iocall_backspace, code);
}


/* Translate an ENDFILE statement.  */

tree
gfc_trans_endfile (gfc_code * code)
{

  return build_filepos (iocall_endfile, code);
}


/* Translate a REWIND statement.  */

tree
gfc_trans_rewind (gfc_code * code)
{

  return build_filepos (iocall_rewind, code);
}


/* Translate the non-IOLENGTH form of an INQUIRE statement.  */

tree
gfc_trans_inquire (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_inquire *p;
  tree tmp;

  gfc_init_block (&block);
  gfc_init_block (&post_block);

  set_error_locus (&block, &code->loc);
  p = code->ext.inquire;

  if (p->unit)
    set_parameter_value (&block, ioparm_unit, p->unit);

  if (p->file)
    set_string (&block, &post_block, ioparm_file, ioparm_file_len, p->file);

  if (p->iostat)
    set_parameter_ref (&block, ioparm_iostat, p->iostat);

  if (p->exist)
    set_parameter_ref (&block, ioparm_exist, p->exist);

  if (p->opened)
    set_parameter_ref (&block, ioparm_opened, p->opened);

  if (p->number)
    set_parameter_ref (&block, ioparm_number, p->number);

  if (p->named)
    set_parameter_ref (&block, ioparm_named, p->named);

  if (p->name)
    set_string (&block, &post_block, ioparm_name, ioparm_name_len, p->name);

  if (p->access)
    set_string (&block, &post_block, ioparm_access,
		ioparm_access_len, p->access);

  if (p->sequential)
    set_string (&block, &post_block, ioparm_sequential,
		ioparm_sequential_len, p->sequential);

  if (p->direct)
    set_string (&block, &post_block, ioparm_direct,
		ioparm_direct_len, p->direct);

  if (p->form)
    set_string (&block, &post_block, ioparm_form, ioparm_form_len, p->form);

  if (p->formatted)
    set_string (&block, &post_block, ioparm_formatted,
		ioparm_formatted_len, p->formatted);

  if (p->unformatted)
    set_string (&block, &post_block, ioparm_unformatted,
		ioparm_unformatted_len, p->unformatted);

  if (p->recl)
    set_parameter_ref (&block, ioparm_recl_out, p->recl);

  if (p->nextrec)
    set_parameter_ref (&block, ioparm_nextrec, p->nextrec);

  if (p->blank)
    set_string (&block, &post_block, ioparm_blank, ioparm_blank_len,
		p->blank);

  if (p->position)
    set_string (&block, &post_block, ioparm_position,
		ioparm_position_len, p->position);

  if (p->action)
    set_string (&block, &post_block, ioparm_action,
		ioparm_action_len, p->action);

  if (p->read)
    set_string (&block, &post_block, ioparm_read, ioparm_read_len, p->read);

  if (p->write)
    set_string (&block, &post_block, ioparm_write,
		ioparm_write_len, p->write);

  if (p->readwrite)
    set_string (&block, &post_block, ioparm_readwrite,
		ioparm_readwrite_len, p->readwrite);

  if (p->delim)
    set_string (&block, &post_block, ioparm_delim, ioparm_delim_len,
		p->delim);

  if (p->pad)
    set_string (&block, &post_block, ioparm_pad, ioparm_pad_len,
                p->pad); 

  if (p->err)
    set_flag (&block, ioparm_err);

  tmp = gfc_build_function_call (iocall_inquire, NULL);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}

static gfc_expr *
gfc_new_nml_name_expr (const char * name)
{
   gfc_expr * nml_name;

   nml_name = gfc_get_expr();
   nml_name->ref = NULL;
   nml_name->expr_type = EXPR_CONSTANT;
   nml_name->ts.kind = gfc_default_character_kind;
   nml_name->ts.type = BT_CHARACTER;
   nml_name->value.character.length = strlen(name);
   nml_name->value.character.string = gfc_getmem (strlen (name) + 1);
   strcpy (nml_name->value.character.string, name);

   return nml_name;
}

/* nml_full_name builds up the fully qualified name of a
   derived type component. */

static char*
nml_full_name (const char* var_name, const char* cmp_name)
{
  int full_name_length;
  char * full_name;

  full_name_length = strlen (var_name) + strlen (cmp_name) + 1;
  full_name = (char*)gfc_getmem (full_name_length + 1);
  strcpy (full_name, var_name);
  full_name = strcat (full_name, "%");
  full_name = strcat (full_name, cmp_name);
  return full_name;
}

/* nml_get_addr_expr builds an address expression from the
   gfc_symbol or gfc_component backend_decl's. An offset is
   provided so that the address of an element of an array of
   derived types is returned. This is used in the runtime to
   determine that span of the derived type. */

static tree
nml_get_addr_expr (gfc_symbol * sym, gfc_component * c,
		   tree base_addr)
{
  tree decl = NULL_TREE;
  tree tmp;
  tree itmp;
  int array_flagged;
  int dummy_arg_flagged;

  if (sym)
    {
      sym->attr.referenced = 1;
      decl = gfc_get_symbol_decl (sym);
    }
  else
    decl = c->backend_decl;

  gcc_assert (decl && ((TREE_CODE (decl) == FIELD_DECL
		     || TREE_CODE (decl) == VAR_DECL
		     || TREE_CODE (decl) == PARM_DECL)
		     || TREE_CODE (decl) == COMPONENT_REF));

  tmp = decl;

  /* Build indirect reference, if dummy argument.  */

  dummy_arg_flagged = POINTER_TYPE_P (TREE_TYPE(tmp));

  itmp = (dummy_arg_flagged) ? gfc_build_indirect_ref (tmp) : tmp;

  /* If an array, set flag and use indirect ref. if built.  */

  array_flagged = (TREE_CODE (TREE_TYPE (itmp)) == ARRAY_TYPE
		   && !TYPE_STRING_FLAG (TREE_TYPE (itmp)));

  if (array_flagged)
    tmp = itmp;

  /* Treat the component of a derived type, using base_addr for
     the derived type.  */

  if (TREE_CODE (decl) == FIELD_DECL)
    tmp = build3 (COMPONENT_REF, TREE_TYPE (tmp),
		  base_addr, tmp, NULL_TREE);

  /* If we have a derived type component, a reference to the first
     element of the array is built.  This is done so that base_addr,
     used in the build of the component reference, always points to
     a RECORD_TYPE.  */

  if (array_flagged)
    tmp = gfc_build_array_ref (tmp, gfc_index_zero_node);

  /* Now build the address expression.  */

  tmp = gfc_build_addr_expr (NULL, tmp);

  /* If scalar dummy, resolve indirect reference now.  */

  if (dummy_arg_flagged && !array_flagged)
    tmp = gfc_build_indirect_ref (tmp);

  gcc_assert (tmp && POINTER_TYPE_P (TREE_TYPE (tmp)));

  return tmp;
}

/* For an object VAR_NAME whose base address is BASE_ADDR, generate a
   call to iocall_set_nml_val.  For derived type variable, recursively
   generate calls to iocall_set_nml_val for each component.  */

#define NML_FIRST_ARG(a) args = gfc_chainon_list (NULL_TREE, a)
#define NML_ADD_ARG(a) args = gfc_chainon_list (args, a)
#define IARG(i) build_int_cst (gfc_array_index_type, i)

static void
transfer_namelist_element (stmtblock_t * block, const char * var_name,
			   gfc_symbol * sym, gfc_component * c,
			   tree base_addr)
{
  gfc_typespec * ts = NULL;
  gfc_array_spec * as = NULL;
  tree addr_expr = NULL;
  tree dt = NULL;
  tree string;
  tree tmp;
  tree args;
  tree dtype;
  int n_dim; 
  int itype;
  int rank = 0;

  gcc_assert (sym || c);

  /* Build the namelist object name.  */

  string = gfc_build_cstring_const (var_name);
  string = gfc_build_addr_expr (pchar_type_node, string);

  /* Build ts, as and data address using symbol or component.  */

  ts = (sym) ? &sym->ts : &c->ts;
  as = (sym) ? sym->as : c->as;

  addr_expr = nml_get_addr_expr (sym, c, base_addr);

  if (as)
    rank = as->rank;

  if (rank)
    {
      dt =  TREE_TYPE ((sym) ? sym->backend_decl : c->backend_decl);
      dtype = gfc_get_dtype (dt);
    }
  else
    {
      itype = GFC_DTYPE_UNKNOWN;

      switch (ts->type)

	{
	case BT_INTEGER:
	  itype = GFC_DTYPE_INTEGER;
	  break;
	case BT_LOGICAL:
	  itype = GFC_DTYPE_LOGICAL;
	  break;
	case BT_REAL:
	  itype = GFC_DTYPE_REAL;
	  break;
	case BT_COMPLEX:
	  itype = GFC_DTYPE_COMPLEX;
	break;
	case BT_DERIVED:
	  itype = GFC_DTYPE_DERIVED;
	  break;
	case BT_CHARACTER:
	  itype = GFC_DTYPE_CHARACTER;
	  break;
	default:
	  gcc_unreachable ();
	}

      dtype = IARG (itype << GFC_DTYPE_TYPE_SHIFT);
    }

  /* Build up the arguments for the transfer call.
     The call for the scalar part transfers:
     (address, name, type, kind or string_length, dtype)  */

  NML_FIRST_ARG (addr_expr);
  NML_ADD_ARG (string);
  NML_ADD_ARG (IARG (ts->kind));

  if (ts->type == BT_CHARACTER)
    NML_ADD_ARG (ts->cl->backend_decl);
  else
    NML_ADD_ARG (convert (gfc_charlen_type_node, integer_zero_node));

  NML_ADD_ARG (dtype);
  tmp = gfc_build_function_call (iocall_set_nml_val, args);
  gfc_add_expr_to_block (block, tmp);

  /* If the object is an array, transfer rank times:
     (null pointer, name, stride, lbound, ubound)  */

  for ( n_dim = 0 ; n_dim < rank ; n_dim++ )
    {
      NML_FIRST_ARG (IARG (n_dim));
      NML_ADD_ARG (GFC_TYPE_ARRAY_STRIDE (dt, n_dim));
      NML_ADD_ARG (GFC_TYPE_ARRAY_LBOUND (dt, n_dim));
      NML_ADD_ARG (GFC_TYPE_ARRAY_UBOUND (dt, n_dim));
      tmp = gfc_build_function_call (iocall_set_nml_val_dim, args);
      gfc_add_expr_to_block (block, tmp);
    }

  if (ts->type == BT_DERIVED)
    {
      gfc_component *cmp;

      /* Provide the RECORD_TYPE to build component references.  */

      tree expr = gfc_build_indirect_ref (addr_expr);

      for (cmp = ts->derived->components; cmp; cmp = cmp->next)
	{
	  char *full_name = nml_full_name (var_name, cmp->name);
	  transfer_namelist_element (block,
				     full_name,
				     NULL, cmp, expr);
	  gfc_free (full_name);
	}
    }
}

#undef IARG
#undef NML_ADD_ARG
#undef NML_FIRST_ARG

/* Create a data transfer statement.  Not all of the fields are valid
   for both reading and writing, but improper use has been filtered
   out by now.  */

static tree
build_dt (tree * function, gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_dt *dt;
  tree tmp;
  gfc_expr *nmlname;
  gfc_namelist *nml;

  gfc_init_block (&block);
  gfc_init_block (&post_block);

  set_error_locus (&block, &code->loc);
  dt = code->ext.dt;

  gcc_assert (dt != NULL);

  if (dt->io_unit)
    {
      if (dt->io_unit->ts.type == BT_CHARACTER)
	{
	  set_string (&block, &post_block, ioparm_internal_unit,
		      ioparm_internal_unit_len, dt->io_unit);
	}
      else
	set_parameter_value (&block, ioparm_unit, dt->io_unit);
    }

  if (dt->rec)
    set_parameter_value (&block, ioparm_rec, dt->rec);

  if (dt->advance)
    set_string (&block, &post_block, ioparm_advance, ioparm_advance_len,
		dt->advance);

  if (dt->format_expr)
    set_string (&block, &post_block, ioparm_format, ioparm_format_len,
		dt->format_expr);

  if (dt->format_label)
    {
      if (dt->format_label == &format_asterisk)
	set_flag (&block, ioparm_list_format);
      else
        set_string (&block, &post_block, ioparm_format,
		    ioparm_format_len, dt->format_label->format);
    }

  if (dt->iostat)
    set_parameter_ref (&block, ioparm_iostat, dt->iostat);

  if (dt->size)
    set_parameter_ref (&block, ioparm_size, dt->size);

  if (dt->err)
    set_flag (&block, ioparm_err);

  if (dt->eor)
    set_flag(&block, ioparm_eor);

  if (dt->end)
    set_flag(&block, ioparm_end);

  if (dt->namelist)
    {
      if (dt->format_expr || dt->format_label)
        gfc_internal_error ("build_dt: format with namelist");

      nmlname = gfc_new_nml_name_expr(dt->namelist->name);

      set_string (&block, &post_block, ioparm_namelist_name,
		  ioparm_namelist_name_len, nmlname);

      if (last_dt == READ)
	set_flag (&block, ioparm_namelist_read_mode);

      for (nml = dt->namelist->namelist; nml; nml = nml->next)
	transfer_namelist_element (&block, nml->sym->name, nml->sym,
				   NULL, NULL);
    }

  tmp = gfc_build_function_call (*function, NULL_TREE);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  return gfc_finish_block (&block);
}


/* Translate the IOLENGTH form of an INQUIRE statement.  We treat
   this as a third sort of data transfer statement, except that
   lengths are summed instead of actually transferring any data.  */

tree
gfc_trans_iolength (gfc_code * code)
{
  stmtblock_t block;
  gfc_inquire *inq;
  tree dt;

  gfc_init_block (&block);

  set_error_locus (&block, &code->loc);

  inq = code->ext.inquire;

  /* First check that preconditions are met.  */
  gcc_assert (inq != NULL);
  gcc_assert (inq->iolength != NULL);

  /* Connect to the iolength variable.  */
  if (inq->iolength)
    set_parameter_ref (&block, ioparm_iolength, inq->iolength);

  /* Actual logic.  */
  last_dt = IOLENGTH;
  dt = build_dt(&iocall_iolength, code);

  gfc_add_expr_to_block (&block, dt);

  return gfc_finish_block (&block);
}


/* Translate a READ statement.  */

tree
gfc_trans_read (gfc_code * code)
{

  last_dt = READ;
  return build_dt (&iocall_read, code);
}


/* Translate a WRITE statement */

tree
gfc_trans_write (gfc_code * code)
{

  last_dt = WRITE;
  return build_dt (&iocall_write, code);
}


/* Finish a data transfer statement.  */

tree
gfc_trans_dt_end (gfc_code * code)
{
  tree function, tmp;
  stmtblock_t block;

  gfc_init_block (&block);

  switch (last_dt)
    {
    case READ:
      function = iocall_read_done;
      break;

    case WRITE:
      function = iocall_write_done;
      break;

    case IOLENGTH:
      function = iocall_iolength_done;
      break;

    default:
      gcc_unreachable ();
    }

  tmp = gfc_build_function_call (function, NULL);
  gfc_add_expr_to_block (&block, tmp);

  if (last_dt != IOLENGTH)
    {
      gcc_assert (code->ext.dt != NULL);
      io_result (&block, code->ext.dt->err,
		 code->ext.dt->end, code->ext.dt->eor);
    }

  return gfc_finish_block (&block);
}

static void
transfer_expr (gfc_se * se, gfc_typespec * ts, tree addr_expr);

/* Given an array field in a derived type variable, generate the code
   for the loop that iterates over array elements, and the code that
   accesses those array elements.  Use transfer_expr to generate code
   for transferring that element.  Because elements may also be
   derived types, transfer_expr and transfer_array_component are mutually
   recursive.  */

static tree
transfer_array_component (tree expr, gfc_component * cm)
{
  tree tmp;
  stmtblock_t body;
  stmtblock_t block;
  gfc_loopinfo loop;
  int n;
  gfc_ss *ss;
  gfc_se se;

  gfc_start_block (&block);
  gfc_init_se (&se, NULL);

  /* Create and initialize Scalarization Status.  Unlike in
     gfc_trans_transfer, we can't simply use gfc_walk_expr to take
     care of this task, because we don't have a gfc_expr at hand.
     Build one manually, as in gfc_trans_subarray_assign.  */

  ss = gfc_get_ss ();
  ss->type = GFC_SS_COMPONENT;
  ss->expr = NULL;
  ss->shape = gfc_get_shape (cm->as->rank);
  ss->next = gfc_ss_terminator;
  ss->data.info.dimen = cm->as->rank;
  ss->data.info.descriptor = expr;
  ss->data.info.data = gfc_conv_array_data (expr);
  ss->data.info.offset = gfc_conv_array_offset (expr);
  for (n = 0; n < cm->as->rank; n++)
    {
      ss->data.info.dim[n] = n;
      ss->data.info.start[n] = gfc_conv_array_lbound (expr, n);
      ss->data.info.stride[n] = gfc_index_one_node;

      mpz_init (ss->shape[n]);
      mpz_sub (ss->shape[n], cm->as->upper[n]->value.integer,
               cm->as->lower[n]->value.integer);
      mpz_add_ui (ss->shape[n], ss->shape[n], 1);
    }

  /* Once we got ss, we use scalarizer to create the loop.  */

  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, ss);
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop);
  gfc_mark_ss_chain_used (ss, 1);
  gfc_start_scalarized_body (&loop, &body);

  gfc_copy_loopinfo_to_se (&se, &loop);
  se.ss = ss;

  /* gfc_conv_tmp_array_ref assumes that se.expr contains the array.  */
  se.expr = expr;
  gfc_conv_tmp_array_ref (&se);

  /* Now se.expr contains an element of the array.  Take the address and pass
     it to the IO routines.  */
  tmp = gfc_build_addr_expr (NULL, se.expr);
  transfer_expr (&se, &cm->ts, tmp);

  /* We are done now with the loop body.  Wrap up the scalarizer and
     return.  */

  gfc_add_block_to_block (&body, &se.pre);
  gfc_add_block_to_block (&body, &se.post);

  gfc_trans_scalarizing_loops (&loop, &body);

  gfc_add_block_to_block (&block, &loop.pre);
  gfc_add_block_to_block (&block, &loop.post);

  for (n = 0; n < cm->as->rank; n++)
    mpz_clear (ss->shape[n]);
  gfc_free (ss->shape);

  gfc_cleanup_loop (&loop);

  return gfc_finish_block (&block);
}

/* Generate the call for a scalar transfer node.  */

static void
transfer_expr (gfc_se * se, gfc_typespec * ts, tree addr_expr)
{
  tree args, tmp, function, arg2, field, expr;
  gfc_component *c;
  int kind;

  kind = ts->kind;
  function = NULL;
  arg2 = NULL;

  switch (ts->type)
    {
    case BT_INTEGER:
      arg2 = build_int_cst (NULL_TREE, kind);
      function = iocall_x_integer;
      break;

    case BT_REAL:
      arg2 = build_int_cst (NULL_TREE, kind);
      function = iocall_x_real;
      break;

    case BT_COMPLEX:
      arg2 = build_int_cst (NULL_TREE, kind);
      function = iocall_x_complex;
      break;

    case BT_LOGICAL:
      arg2 = build_int_cst (NULL_TREE, kind);
      function = iocall_x_logical;
      break;

    case BT_CHARACTER:
      if (se->string_length)
	arg2 = se->string_length;
      else
	{
	  tmp = gfc_build_indirect_ref (addr_expr);
	  gcc_assert (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE);
	  arg2 = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (tmp)));
	}
      function = iocall_x_character;
      break;

    case BT_DERIVED:
      /* Recurse into the elements of the derived type.  */
      expr = gfc_evaluate_now (addr_expr, &se->pre);
      expr = gfc_build_indirect_ref (expr);

      for (c = ts->derived->components; c; c = c->next)
	{
	  field = c->backend_decl;
	  gcc_assert (field && TREE_CODE (field) == FIELD_DECL);

	  tmp = build3 (COMPONENT_REF, TREE_TYPE (field), expr, field,
			NULL_TREE);

          if (c->dimension)
            {
              tmp = transfer_array_component (tmp, c);
              gfc_add_expr_to_block (&se->pre, tmp);
            }
          else
            {
              if (!c->pointer)
                tmp = gfc_build_addr_expr (NULL, tmp);
              transfer_expr (se, &c->ts, tmp);
            }
	}
      return;

    default:
      internal_error ("Bad IO basetype (%d)", ts->type);
    }

  args = gfc_chainon_list (NULL_TREE, addr_expr);
  args = gfc_chainon_list (args, arg2);

  tmp = gfc_build_function_call (function, args);
  gfc_add_expr_to_block (&se->pre, tmp);
  gfc_add_block_to_block (&se->pre, &se->post);

}


/* gfc_trans_transfer()-- Translate a TRANSFER code node */

tree
gfc_trans_transfer (gfc_code * code)
{
  stmtblock_t block, body;
  gfc_loopinfo loop;
  gfc_expr *expr;
  gfc_ss *ss;
  gfc_se se;
  tree tmp;

  gfc_start_block (&block);

  expr = code->expr;
  ss = gfc_walk_expr (expr);

  gfc_init_se (&se, NULL);

  if (ss == gfc_ss_terminator)
    gfc_init_block (&body);
  else
    {
      /* Initialize the scalarizer.  */
      gfc_init_loopinfo (&loop);
      gfc_add_ss_to_loop (&loop, ss);

      /* Initialize the loop.  */
      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop);

      /* The main loop body.  */
      gfc_mark_ss_chain_used (ss, 1);
      gfc_start_scalarized_body (&loop, &body);

      gfc_copy_loopinfo_to_se (&se, &loop);
      se.ss = ss;
    }

  gfc_conv_expr_reference (&se, expr);

  transfer_expr (&se, &expr->ts, se.expr);

  gfc_add_block_to_block (&body, &se.pre);
  gfc_add_block_to_block (&body, &se.post);

  if (se.ss == NULL)
    tmp = gfc_finish_block (&body);
  else
    {
      gcc_assert (se.ss == gfc_ss_terminator);
      gfc_trans_scalarizing_loops (&loop, &body);

      gfc_add_block_to_block (&loop.pre, &loop.post);
      tmp = gfc_finish_block (&loop.pre);
      gfc_cleanup_loop (&loop);
    }

  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}

#include "gt-fortran-trans-io.h"

