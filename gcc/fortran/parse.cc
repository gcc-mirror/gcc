/* Main parser.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include <setjmp.h>
#include "match.h"
#include "parse.h"
#include "tree-core.h"
#include "omp-general.h"

/* Current statement label.  Zero means no statement label.  Because new_st
   can get wiped during statement matching, we have to keep it separate.  */

gfc_st_label *gfc_statement_label;

static locus label_locus;
static jmp_buf eof_buf;

/* Respectively pointer and content of the current interface body being parsed
   as they were at the beginning of decode_statement.  Used to restore the
   interface to its previous state in case a parsed statement is rejected after
   some symbols have been added to the interface.  */
static gfc_interface **current_interface_ptr = nullptr;
static gfc_interface *previous_interface_head = nullptr;

gfc_state_data *gfc_state_stack;
static bool last_was_use_stmt = false;
bool in_exec_part;

/* TODO: Re-order functions to kill these forward decls.  */
static void check_statement_label (gfc_statement);
static void undo_new_statement (void);
static void reject_statement (void);


/* A sort of half-matching function.  We try to match the word on the
   input with the passed string.  If this succeeds, we call the
   keyword-dependent matching function that will match the rest of the
   statement.  For single keywords, the matching subroutine is
   gfc_match_eos().  */

static match
match_word (const char *str, match (*subr) (void), locus *old_locus)
{
  match m;

  if (str != NULL)
    {
      m = gfc_match (str);
      if (m != MATCH_YES)
	return m;
    }

  m = (*subr) ();

  if (m != MATCH_YES)
    {
      gfc_current_locus = *old_locus;
      reject_statement ();
    }

  return m;
}


/* Like match_word, but if str is matched, set a flag that it
   was matched.  */
static match
match_word_omp_simd (const char *str, match (*subr) (void), locus *old_locus,
		     bool *simd_matched)
{
  match m;

  if (str != NULL)
    {
      m = gfc_match (str);
      if (m != MATCH_YES)
	return m;
      *simd_matched = true;
    }

  m = (*subr) ();

  if (m != MATCH_YES)
    {
      gfc_current_locus = *old_locus;
      reject_statement ();
    }

  return m;
}


/* Load symbols from all USE statements encountered in this scoping unit.  */

static void
use_modules (void)
{
  gfc_error_buffer old_error;

  gfc_push_error (&old_error);
  gfc_buffer_error (false);
  gfc_use_modules ();
  gfc_buffer_error (true);
  gfc_pop_error (&old_error);
  gfc_commit_symbols ();
  gfc_warning_check ();
  gfc_current_ns->old_equiv = gfc_current_ns->equiv;
  gfc_current_ns->old_data = gfc_current_ns->data;
  last_was_use_stmt = false;
}


/* Figure out what the next statement is, (mostly) regardless of
   proper ordering.  The do...while(0) is there to prevent if/else
   ambiguity.  */

#define match(keyword, subr, st)				\
    do {							\
      if (match_word (keyword, subr, &old_locus) == MATCH_YES)	\
	return st;						\
      else							\
	undo_new_statement ();				  	\
    } while (0)


/* This is a specialist version of decode_statement that is used
   for the specification statements in a function, whose
   characteristics are deferred into the specification statements.
   eg.:  INTEGER (king = mykind) foo ()
	 USE mymodule, ONLY mykind.....
   The KIND parameter needs a return after USE or IMPORT, whereas
   derived type declarations can occur anywhere, up the executable
   block.  ST_GET_FCN_CHARACTERISTICS is returned when we have run
   out of the correct kind of specification statements.  */
static gfc_statement
decode_specification_statement (void)
{
  gfc_statement st;
  locus old_locus;
  char c;

  if (gfc_match_eos () == MATCH_YES)
    return ST_NONE;

  old_locus = gfc_current_locus;

  if (match_word ("use", gfc_match_use, &old_locus) == MATCH_YES)
    {
      last_was_use_stmt = true;
      return ST_USE;
    }
  else
    {
      undo_new_statement ();
      if (last_was_use_stmt)
	use_modules ();
    }

  match ("import", gfc_match_import, ST_IMPORT);

  if (gfc_current_block ()->result->ts.type != BT_DERIVED)
    goto end_of_block;

  match (NULL, gfc_match_st_function, ST_STATEMENT_FUNCTION);
  match (NULL, gfc_match_data_decl, ST_DATA_DECL);
  match (NULL, gfc_match_enumerator_def, ST_ENUMERATOR);

  /* General statement matching: Instead of testing every possible
     statement, we eliminate most possibilities by peeking at the
     first character.  */

  c = gfc_peek_ascii_char ();

  switch (c)
    {
    case 'a':
      match ("abstract% interface", gfc_match_abstract_interface,
	     ST_INTERFACE);
      match ("allocatable", gfc_match_allocatable, ST_ATTR_DECL);
      match ("asynchronous", gfc_match_asynchronous, ST_ATTR_DECL);
      match ("automatic", gfc_match_automatic, ST_ATTR_DECL);
      break;

    case 'b':
      match (NULL, gfc_match_bind_c_stmt, ST_ATTR_DECL);
      break;

    case 'c':
      match ("codimension", gfc_match_codimension, ST_ATTR_DECL);
      match ("contiguous", gfc_match_contiguous, ST_ATTR_DECL);
      break;

    case 'd':
      match ("data", gfc_match_data, ST_DATA);
      match ("dimension", gfc_match_dimension, ST_ATTR_DECL);
      break;

    case 'e':
      match ("enum , bind ( c )", gfc_match_enum, ST_ENUM);
      match ("entry% ", gfc_match_entry, ST_ENTRY);
      match ("equivalence", gfc_match_equivalence, ST_EQUIVALENCE);
      match ("external", gfc_match_external, ST_ATTR_DECL);
      break;

    case 'f':
      match ("format", gfc_match_format, ST_FORMAT);
      break;

    case 'g':
      break;

    case 'i':
      match ("implicit", gfc_match_implicit, ST_IMPLICIT);
      match ("implicit% none", gfc_match_implicit_none, ST_IMPLICIT_NONE);
      match ("interface", gfc_match_interface, ST_INTERFACE);
      match ("intent", gfc_match_intent, ST_ATTR_DECL);
      match ("intrinsic", gfc_match_intrinsic, ST_ATTR_DECL);
      break;

    case 'm':
      break;

    case 'n':
      match ("namelist", gfc_match_namelist, ST_NAMELIST);
      break;

    case 'o':
      match ("optional", gfc_match_optional, ST_ATTR_DECL);
      break;

    case 'p':
      match ("parameter", gfc_match_parameter, ST_PARAMETER);
      match ("pointer", gfc_match_pointer, ST_ATTR_DECL);
      if (gfc_match_private (&st) == MATCH_YES)
	return st;
      match ("procedure", gfc_match_procedure, ST_PROCEDURE);
      if (gfc_match_public (&st) == MATCH_YES)
	return st;
      match ("protected", gfc_match_protected, ST_ATTR_DECL);
      break;

    case 'r':
      break;

    case 's':
      match ("save", gfc_match_save, ST_ATTR_DECL);
      match ("static", gfc_match_static, ST_ATTR_DECL);
      match ("structure", gfc_match_structure_decl, ST_STRUCTURE_DECL);
      break;

    case 't':
      match ("target", gfc_match_target, ST_ATTR_DECL);
      match ("type", gfc_match_derived_decl, ST_DERIVED_DECL);
      break;

    case 'u':
      break;

    case 'v':
      match ("value", gfc_match_value, ST_ATTR_DECL);
      match ("volatile", gfc_match_volatile, ST_ATTR_DECL);
      break;

    case 'w':
      break;
    }

  /* This is not a specification statement.  See if any of the matchers
     has stored an error message of some sort.  */

end_of_block:
  gfc_clear_error ();
  gfc_buffer_error (false);
  gfc_current_locus = old_locus;

  return ST_GET_FCN_CHARACTERISTICS;
}


/* Tells whether gfc_get_current_interface_head can be used safely.  */

static bool
current_interface_valid_p ()
{
  switch (current_interface.type)
    {
    case INTERFACE_INTRINSIC_OP:
      return current_interface.ns != nullptr;

    case INTERFACE_GENERIC:
    case INTERFACE_DTIO:
      return current_interface.sym != nullptr;

    case INTERFACE_USER_OP:
      return current_interface.uop != nullptr;

    default:
      return false;
    }
}


/* Return a pointer to the interface currently being parsed, or nullptr if
   we are not currently parsing an interface body.  */

static gfc_interface **
get_current_interface_ptr ()
{
  if (current_interface_valid_p ())
    {
      gfc_interface *& ifc_ptr = gfc_current_interface_head ();
      return &ifc_ptr;
    }
  else
    return nullptr;
}


static bool in_specification_block;

/* This is the primary 'decode_statement'.  */
static gfc_statement
decode_statement (void)
{
  gfc_statement st;
  locus old_locus;
  match m = MATCH_NO;
  char c;

  gfc_enforce_clean_symbol_state ();

  gfc_clear_error ();	/* Clear any pending errors.  */
  gfc_clear_warning ();	/* Clear any pending warnings.  */

  current_interface_ptr = get_current_interface_ptr ();
  previous_interface_head = current_interface_ptr == nullptr
			    ? nullptr
			    : *current_interface_ptr;

  gfc_matching_function = false;

  if (gfc_match_eos () == MATCH_YES)
    return ST_NONE;

  if (gfc_current_state () == COMP_FUNCTION
	&& gfc_current_block ()->result->ts.kind == -1)
    return decode_specification_statement ();

  old_locus = gfc_current_locus;

  c = gfc_peek_ascii_char ();

  if (c == 'u')
    {
      if (match_word ("use", gfc_match_use, &old_locus) == MATCH_YES)
	{
	  last_was_use_stmt = true;
	  return ST_USE;
	}
      else
	undo_new_statement ();
    }

  if (last_was_use_stmt)
    use_modules ();

  /* Try matching a data declaration or function declaration. The
      input "REALFUNCTIONA(N)" can mean several things in different
      contexts, so it (and its relatives) get special treatment.  */

  if (gfc_current_state () == COMP_NONE
      || gfc_current_state () == COMP_INTERFACE
      || gfc_current_state () == COMP_CONTAINS)
    {
      gfc_matching_function = true;
      m = gfc_match_function_decl ();
      if (m == MATCH_YES)
	return ST_FUNCTION;
      else if (m == MATCH_ERROR)
	reject_statement ();
      else
	gfc_undo_symbols ();
      gfc_current_locus = old_locus;
    }
  gfc_matching_function = false;

  /* Legacy parameter statements are ambiguous with assignments so try parameter
     first.  */
  match ("parameter", gfc_match_parameter, ST_PARAMETER);

  /* Match statements whose error messages are meant to be overwritten
     by something better.  */

  match (NULL, gfc_match_assignment, ST_ASSIGNMENT);
  match (NULL, gfc_match_pointer_assignment, ST_POINTER_ASSIGNMENT);

  if (in_specification_block)
    {
      m = match_word (NULL, gfc_match_st_function, &old_locus);
      if (m == MATCH_YES)
	return ST_STATEMENT_FUNCTION;
    }

  if (!(in_specification_block && m == MATCH_ERROR))
    {
      match (NULL, gfc_match_ptr_fcn_assign, ST_ASSIGNMENT);
    }

  match (NULL, gfc_match_data_decl, ST_DATA_DECL);
  match (NULL, gfc_match_enumerator_def, ST_ENUMERATOR);

  /* Try to match a subroutine statement, which has the same optional
     prefixes that functions can have.  */

  if (gfc_match_subroutine () == MATCH_YES)
    return ST_SUBROUTINE;
  gfc_undo_symbols ();
  gfc_current_locus = old_locus;

  if (gfc_match_submod_proc () == MATCH_YES)
    {
      if (gfc_new_block->attr.subroutine)
	return ST_SUBROUTINE;
      else if (gfc_new_block->attr.function)
	return ST_FUNCTION;
    }
  gfc_undo_symbols ();
  gfc_current_locus = old_locus;

  /* Check for the IF, DO, SELECT, WHERE, FORALL, CRITICAL, BLOCK and ASSOCIATE
     statements, which might begin with a block label.  The match functions for
     these statements are unusual in that their keyword is not seen before
     the matcher is called.  */

  if (gfc_match_if (&st) == MATCH_YES)
    return st;
  gfc_undo_symbols ();
  gfc_current_locus = old_locus;

  if (gfc_match_where (&st) == MATCH_YES)
    return st;
  gfc_undo_symbols ();
  gfc_current_locus = old_locus;

  if (gfc_match_forall (&st) == MATCH_YES)
    return st;
  gfc_undo_symbols ();
  gfc_current_locus = old_locus;

  /* Try to match TYPE as an alias for PRINT.  */
  if (gfc_match_type (&st) == MATCH_YES)
    return st;
  gfc_undo_symbols ();
  gfc_current_locus = old_locus;

  match (NULL, gfc_match_do, ST_DO);
  match (NULL, gfc_match_block, ST_BLOCK);
  match (NULL, gfc_match_associate, ST_ASSOCIATE);
  match (NULL, gfc_match_critical, ST_CRITICAL);
  match (NULL, gfc_match_select, ST_SELECT_CASE);
  match (NULL, gfc_match_select_type, ST_SELECT_TYPE);
  match (NULL, gfc_match_select_rank, ST_SELECT_RANK);

  /* General statement matching: Instead of testing every possible
     statement, we eliminate most possibilities by peeking at the
     first character.  */

  switch (c)
    {
    case 'a':
      match ("abstract% interface", gfc_match_abstract_interface,
	     ST_INTERFACE);
      match ("allocate", gfc_match_allocate, ST_ALLOCATE);
      match ("allocatable", gfc_match_allocatable, ST_ATTR_DECL);
      match ("assign", gfc_match_assign, ST_LABEL_ASSIGNMENT);
      match ("asynchronous", gfc_match_asynchronous, ST_ATTR_DECL);
      match ("automatic", gfc_match_automatic, ST_ATTR_DECL);
      break;

    case 'b':
      match ("backspace", gfc_match_backspace, ST_BACKSPACE);
      match ("block data", gfc_match_block_data, ST_BLOCK_DATA);
      match (NULL, gfc_match_bind_c_stmt, ST_ATTR_DECL);
      break;

    case 'c':
      match ("call", gfc_match_call, ST_CALL);
      match ("change% team", gfc_match_change_team, ST_CHANGE_TEAM);
      match ("close", gfc_match_close, ST_CLOSE);
      match ("continue", gfc_match_continue, ST_CONTINUE);
      match ("contiguous", gfc_match_contiguous, ST_ATTR_DECL);
      match ("cycle", gfc_match_cycle, ST_CYCLE);
      match ("case", gfc_match_case, ST_CASE);
      match ("common", gfc_match_common, ST_COMMON);
      match ("contains", gfc_match_eos, ST_CONTAINS);
      match ("class", gfc_match_class_is, ST_CLASS_IS);
      match ("codimension", gfc_match_codimension, ST_ATTR_DECL);
      break;

    case 'd':
      match ("deallocate", gfc_match_deallocate, ST_DEALLOCATE);
      match ("data", gfc_match_data, ST_DATA);
      match ("dimension", gfc_match_dimension, ST_ATTR_DECL);
      break;

    case 'e':
      match ("end file", gfc_match_endfile, ST_END_FILE);
      match ("end team", gfc_match_end_team, ST_END_TEAM);
      match ("exit", gfc_match_exit, ST_EXIT);
      match ("else", gfc_match_else, ST_ELSE);
      match ("else where", gfc_match_elsewhere, ST_ELSEWHERE);
      match ("else if", gfc_match_elseif, ST_ELSEIF);
      match ("error% stop", gfc_match_error_stop, ST_ERROR_STOP);
      match ("enum , bind ( c )", gfc_match_enum, ST_ENUM);

      if (gfc_match_end (&st) == MATCH_YES)
	return st;

      match ("entry% ", gfc_match_entry, ST_ENTRY);
      match ("equivalence", gfc_match_equivalence, ST_EQUIVALENCE);
      match ("external", gfc_match_external, ST_ATTR_DECL);
      match ("event% post", gfc_match_event_post, ST_EVENT_POST);
      match ("event% wait", gfc_match_event_wait, ST_EVENT_WAIT);
      break;

    case 'f':
      match ("fail% image", gfc_match_fail_image, ST_FAIL_IMAGE);
      match ("final", gfc_match_final_decl, ST_FINAL);
      match ("flush", gfc_match_flush, ST_FLUSH);
      match ("form% team", gfc_match_form_team, ST_FORM_TEAM);
      match ("format", gfc_match_format, ST_FORMAT);
      break;

    case 'g':
      match ("generic", gfc_match_generic, ST_GENERIC);
      match ("go to", gfc_match_goto, ST_GOTO);
      break;

    case 'i':
      match ("inquire", gfc_match_inquire, ST_INQUIRE);
      match ("implicit", gfc_match_implicit, ST_IMPLICIT);
      match ("implicit% none", gfc_match_implicit_none, ST_IMPLICIT_NONE);
      match ("import", gfc_match_import, ST_IMPORT);
      match ("interface", gfc_match_interface, ST_INTERFACE);
      match ("intent", gfc_match_intent, ST_ATTR_DECL);
      match ("intrinsic", gfc_match_intrinsic, ST_ATTR_DECL);
      break;

    case 'l':
      match ("lock", gfc_match_lock, ST_LOCK);
      break;

    case 'm':
      match ("map", gfc_match_map, ST_MAP);
      match ("module% procedure", gfc_match_modproc, ST_MODULE_PROC);
      match ("module", gfc_match_module, ST_MODULE);
      break;

    case 'n':
      match ("nullify", gfc_match_nullify, ST_NULLIFY);
      match ("namelist", gfc_match_namelist, ST_NAMELIST);
      break;

    case 'o':
      match ("open", gfc_match_open, ST_OPEN);
      match ("optional", gfc_match_optional, ST_ATTR_DECL);
      break;

    case 'p':
      match ("print", gfc_match_print, ST_WRITE);
      match ("pause", gfc_match_pause, ST_PAUSE);
      match ("pointer", gfc_match_pointer, ST_ATTR_DECL);
      if (gfc_match_private (&st) == MATCH_YES)
	return st;
      match ("procedure", gfc_match_procedure, ST_PROCEDURE);
      match ("program", gfc_match_program, ST_PROGRAM);
      if (gfc_match_public (&st) == MATCH_YES)
	return st;
      match ("protected", gfc_match_protected, ST_ATTR_DECL);
      break;

    case 'r':
      match ("rank", gfc_match_rank_is, ST_RANK);
      match ("read", gfc_match_read, ST_READ);
      match ("return", gfc_match_return, ST_RETURN);
      match ("rewind", gfc_match_rewind, ST_REWIND);
      break;

    case 's':
      match ("structure", gfc_match_structure_decl, ST_STRUCTURE_DECL);
      match ("sequence", gfc_match_eos, ST_SEQUENCE);
      match ("stop", gfc_match_stop, ST_STOP);
      match ("save", gfc_match_save, ST_ATTR_DECL);
      match ("static", gfc_match_static, ST_ATTR_DECL);
      match ("submodule", gfc_match_submodule, ST_SUBMODULE);
      match ("sync% all", gfc_match_sync_all, ST_SYNC_ALL);
      match ("sync% images", gfc_match_sync_images, ST_SYNC_IMAGES);
      match ("sync% memory", gfc_match_sync_memory, ST_SYNC_MEMORY);
      match ("sync% team", gfc_match_sync_team, ST_SYNC_TEAM);
      break;

    case 't':
      match ("target", gfc_match_target, ST_ATTR_DECL);
      match ("type", gfc_match_derived_decl, ST_DERIVED_DECL);
      match ("type% is", gfc_match_type_is, ST_TYPE_IS);
      break;

    case 'u':
      match ("union", gfc_match_union, ST_UNION);
      match ("unlock", gfc_match_unlock, ST_UNLOCK);
      break;

    case 'v':
      match ("value", gfc_match_value, ST_ATTR_DECL);
      match ("volatile", gfc_match_volatile, ST_ATTR_DECL);
      break;

    case 'w':
      match ("wait", gfc_match_wait, ST_WAIT);
      match ("write", gfc_match_write, ST_WRITE);
      break;
    }

  /* All else has failed, so give up.  See if any of the matchers has
     stored an error message of some sort.  Suppress the "Unclassifiable
     statement" if a previous error message was emitted, e.g., by
     gfc_error_now ().  */
  if (!gfc_error_check ())
    {
      int ecnt;
      gfc_get_errors (NULL, &ecnt);
      if (ecnt <= 0)
        gfc_error_now ("Unclassifiable statement at %C");
    }

  reject_statement ();

  gfc_error_recovery ();

  return ST_NONE;
}

/* Like match and if spec_only, goto do_spec_only without actually
   matching.  */
/* If the directive matched but the clauses failed, do not start
   matching the next directive in the same switch statement. */
#define matcha(keyword, subr, st)				\
    do {							\
      match m2;							\
      if (spec_only && gfc_match (keyword) == MATCH_YES)	\
	goto do_spec_only;					\
      else if ((m2 = match_word (keyword, subr, &old_locus))	\
	       == MATCH_YES)					\
	return st;						\
      else if (m2 == MATCH_ERROR)				\
	goto error_handling;					\
      else							\
	undo_new_statement ();				  	\
    } while (0)

static gfc_statement
decode_oacc_directive (void)
{
  locus old_locus;
  char c;
  bool spec_only = false;

  gfc_enforce_clean_symbol_state ();

  gfc_clear_error ();   /* Clear any pending errors.  */
  gfc_clear_warning (); /* Clear any pending warnings.  */

  gfc_matching_function = false;

  if (gfc_current_state () == COMP_FUNCTION
      && gfc_current_block ()->result->ts.kind == -1)
    spec_only = true;

  old_locus = gfc_current_locus;

  /* General OpenACC directive matching: Instead of testing every possible
     statement, we eliminate most possibilities by peeking at the
     first character.  */

  c = gfc_peek_ascii_char ();

  switch (c)
    {
    case 'r':
      matcha ("routine", gfc_match_oacc_routine, ST_OACC_ROUTINE);
      break;
    }

  gfc_unset_implicit_pure (NULL);
  if (gfc_pure (NULL))
    {
      gfc_error_now ("OpenACC directives other than ROUTINE may not appear in PURE "
		     "procedures at %C");
      goto error_handling;
    }

  switch (c)
    {
    case 'a':
      matcha ("atomic", gfc_match_oacc_atomic, ST_OACC_ATOMIC);
      break;
    case 'c':
      matcha ("cache", gfc_match_oacc_cache, ST_OACC_CACHE);
      break;
    case 'd':
      matcha ("data", gfc_match_oacc_data, ST_OACC_DATA);
      match ("declare", gfc_match_oacc_declare, ST_OACC_DECLARE);
      break;
    case 'e':
      matcha ("end atomic", gfc_match_omp_eos_error, ST_OACC_END_ATOMIC);
      matcha ("end data", gfc_match_omp_eos_error, ST_OACC_END_DATA);
      matcha ("end host_data", gfc_match_omp_eos_error, ST_OACC_END_HOST_DATA);
      matcha ("end kernels loop", gfc_match_omp_eos_error, ST_OACC_END_KERNELS_LOOP);
      matcha ("end kernels", gfc_match_omp_eos_error, ST_OACC_END_KERNELS);
      matcha ("end loop", gfc_match_omp_eos_error, ST_OACC_END_LOOP);
      matcha ("end parallel loop", gfc_match_omp_eos_error,
	      ST_OACC_END_PARALLEL_LOOP);
      matcha ("end parallel", gfc_match_omp_eos_error, ST_OACC_END_PARALLEL);
      matcha ("end serial loop", gfc_match_omp_eos_error,
	      ST_OACC_END_SERIAL_LOOP);
      matcha ("end serial", gfc_match_omp_eos_error, ST_OACC_END_SERIAL);
      matcha ("enter data", gfc_match_oacc_enter_data, ST_OACC_ENTER_DATA);
      matcha ("exit data", gfc_match_oacc_exit_data, ST_OACC_EXIT_DATA);
      break;
    case 'h':
      matcha ("host_data", gfc_match_oacc_host_data, ST_OACC_HOST_DATA);
      break;
    case 'p':
      matcha ("parallel loop", gfc_match_oacc_parallel_loop,
	      ST_OACC_PARALLEL_LOOP);
      matcha ("parallel", gfc_match_oacc_parallel, ST_OACC_PARALLEL);
      break;
    case 'k':
      matcha ("kernels loop", gfc_match_oacc_kernels_loop,
	      ST_OACC_KERNELS_LOOP);
      matcha ("kernels", gfc_match_oacc_kernels, ST_OACC_KERNELS);
      break;
    case 'l':
      matcha ("loop", gfc_match_oacc_loop, ST_OACC_LOOP);
      break;
    case 's':
      matcha ("serial loop", gfc_match_oacc_serial_loop, ST_OACC_SERIAL_LOOP);
      matcha ("serial", gfc_match_oacc_serial, ST_OACC_SERIAL);
      break;
    case 'u':
      matcha ("update", gfc_match_oacc_update, ST_OACC_UPDATE);
      break;
    case 'w':
      matcha ("wait", gfc_match_oacc_wait, ST_OACC_WAIT);
      break;
    }

  /* Directive not found or stored an error message.
     Check and give up.  */

 error_handling:
  if (gfc_error_check () == 0)
    gfc_error_now ("Unclassifiable OpenACC directive at %C");

  reject_statement ();

  gfc_error_recovery ();

  return ST_NONE;

 do_spec_only:
  reject_statement ();
  gfc_clear_error ();
  gfc_buffer_error (false);
  gfc_current_locus = old_locus;
  return ST_GET_FCN_CHARACTERISTICS;
}

/* Checks for the ST_OMP_ALLOCATE. First, check whether all list items
   are allocatables/pointers - and if so, assume it is associated with a Fortran
   ALLOCATE stmt.  If not, do some initial parsing-related checks and append
   namelist to namespace.
   The check follows OpenMP 5.1 by requiring an executable stmt or OpenMP
   construct before a directive associated with an allocate statement
   (-> ST_OMP_ALLOCATE_EXEC); instead of showing an error, conversion of
   ST_OMP_ALLOCATE -> ST_OMP_ALLOCATE_EXEC would be an alternative.  */

bool
check_omp_allocate_stmt (locus *loc)
{
  gfc_omp_namelist *n;

  if (new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->sym == NULL)
    {
      gfc_error ("%qs directive at %L must either have a variable argument or, "
		 "if associated with an ALLOCATE stmt, must be preceded by an "
		 "executable statement or OpenMP construct",
		 gfc_ascii_statement (ST_OMP_ALLOCATE), loc);
      return false;
    }
  bool has_allocatable = false;
  bool has_non_allocatable = false;
  for (n = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]; n; n = n->next)
    {
      if (n->expr)
	{
	  gfc_error ("Structure-component expression at %L in %qs directive not"
		     " permitted in declarative directive; as directive "
		     "associated with an ALLOCATE stmt it must be preceded by "
		     "an executable statement or OpenMP construct",
		      &n->expr->where, gfc_ascii_statement (ST_OMP_ALLOCATE));
	  return false;
	}
      /* Procedure pointers are not allocatable; hence, we do not regard them as
	 pointers here - and reject them later in gfc_resolve_omp_allocate.  */
      bool alloc_ptr;
      if (n->sym->ts.type == BT_CLASS && n->sym->attr.class_ok)
	alloc_ptr = (CLASS_DATA (n->sym)->attr.allocatable
		     || CLASS_DATA (n->sym)->attr.class_pointer);
      else
	alloc_ptr = n->sym->attr.allocatable || n->sym->attr.pointer;
      if (alloc_ptr
	  || (n->sym->ns && n->sym->ns->proc_name
	      && (n->sym->ns->proc_name->attr.allocatable
		  || n->sym->ns->proc_name->attr.pointer)))
	has_allocatable = true;
      else
	has_non_allocatable = true;
    }
  /* All allocatables - assume it is allocated with an ALLOCATE stmt.  */
  if (has_allocatable && !has_non_allocatable)
    {
      gfc_error ("%qs directive at %L associated with an ALLOCATE stmt must be "
		 "preceded by an executable statement or OpenMP construct; "
		 "note the variables in the list all have the allocatable or "
		 "pointer attribute", gfc_ascii_statement (ST_OMP_ALLOCATE),
		 loc);
      return false;
    }
  if (!gfc_current_ns->omp_allocate)
    gfc_current_ns->omp_allocate
      = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE];
  else
    {
      for (n = gfc_current_ns->omp_allocate; n->next; n = n->next)
	;
      n->next = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE];
    }
  new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE] = NULL;
  gfc_free_omp_clauses (new_st.ext.omp_clauses);
  return true;
}


/* Like match, but set a flag simd_matched if keyword matched
   and if spec_only, goto do_spec_only without actually matching.  */
#define matchs(keyword, subr, st)				\
    do {							\
      match m2;							\
      if (spec_only && gfc_match (keyword) == MATCH_YES)	\
	goto do_spec_only;					\
      if ((m2 = match_word_omp_simd (keyword, subr, &old_locus,	\
			       &simd_matched)) == MATCH_YES)	\
	{							\
	  ret = st;						\
	  goto finish;						\
	}							\
      else if (m2 == MATCH_ERROR)				\
	goto error_handling;					\
      else							\
	undo_new_statement ();				  	\
    } while (0)

/* Like match, but don't match anything if not -fopenmp
   and if spec_only, goto do_spec_only without actually matching.  */
/* If the directive matched but the clauses failed, do not start
   matching the next directive in the same switch statement. */
#define matcho(keyword, subr, st)				\
    do {							\
      match m2;							\
      if (!flag_openmp)						\
	;							\
      else if (spec_only && gfc_match (keyword) == MATCH_YES)	\
	goto do_spec_only;					\
      else if ((m2 = match_word (keyword, subr, &old_locus))	\
	       == MATCH_YES)					\
	{							\
	  ret = st;						\
	  goto finish;						\
	}							\
      else if (m2 == MATCH_ERROR)				\
	goto error_handling;					\
      else							\
	undo_new_statement ();				  	\
    } while (0)

/* Like match, but set a flag simd_matched if keyword matched.  */
#define matchds(keyword, subr, st)				\
    do {							\
      match m2;							\
      if ((m2 = match_word_omp_simd (keyword, subr, &old_locus,	\
			       &simd_matched)) == MATCH_YES)	\
	{							\
	  ret = st;						\
	  goto finish;						\
	}							\
      else if (m2 == MATCH_ERROR)				\
	goto error_handling;					\
      else							\
	undo_new_statement ();				  	\
    } while (0)

/* Like match, but don't match anything if not -fopenmp.  */
#define matchdo(keyword, subr, st)				\
    do {							\
      match m2;							\
      if (!flag_openmp)						\
	;							\
      else if ((m2 = match_word (keyword, subr, &old_locus))	\
	       == MATCH_YES)					\
	{							\
	  ret = st;						\
	  goto finish;						\
	}							\
      else if (m2 == MATCH_ERROR)				\
	goto error_handling;					\
      else							\
	undo_new_statement ();				  	\
    } while (0)

static gfc_statement
decode_omp_directive (void)
{
  locus old_locus;
  char c;
  bool simd_matched = false;
  bool spec_only = false;
  gfc_statement ret = ST_NONE;
  bool pure_ok = true;

  gfc_enforce_clean_symbol_state ();

  gfc_clear_error ();	/* Clear any pending errors.  */
  gfc_clear_warning ();	/* Clear any pending warnings.  */

  gfc_matching_function = false;

  if (gfc_current_state () == COMP_FUNCTION
      && gfc_current_block ()->result->ts.kind == -1)
    spec_only = true;

  old_locus = gfc_current_locus;

  /* General OpenMP directive matching: Instead of testing every possible
     statement, we eliminate most possibilities by peeking at the
     first character.  */

  c = gfc_peek_ascii_char ();

  /* match is for directives that should be recognized only if
     -fopenmp, matchs for directives that should be recognized
     if either -fopenmp or -fopenmp-simd.
     Handle only the directives allowed in PURE procedures
     first (those also shall not turn off implicit pure).  */
  switch (c)
    {
    case 'a':
      /* For -fopenmp-simd, ignore 'assumes'; note no clause starts with 's'. */
      if (!flag_openmp && gfc_match ("assumes") == MATCH_YES)
	break;
      matcho ("assumes", gfc_match_omp_assumes, ST_OMP_ASSUMES);
      matchs ("assume", gfc_match_omp_assume, ST_OMP_ASSUME);
      break;
    case 'd':
      matchds ("declare reduction", gfc_match_omp_declare_reduction,
	       ST_OMP_DECLARE_REDUCTION);
      matchds ("declare simd", gfc_match_omp_declare_simd,
	       ST_OMP_DECLARE_SIMD);
      matchdo ("declare target", gfc_match_omp_declare_target,
	       ST_OMP_DECLARE_TARGET);
      matchdo ("declare variant", gfc_match_omp_declare_variant,
	       ST_OMP_DECLARE_VARIANT);
      break;
    case 'e':
      matchs ("end assume", gfc_match_omp_eos_error, ST_OMP_END_ASSUME);
      matchs ("end simd", gfc_match_omp_eos_error, ST_OMP_END_SIMD);
      matcho ("error", gfc_match_omp_error, ST_OMP_ERROR);
      break;
    case 's':
      matchs ("scan", gfc_match_omp_scan, ST_OMP_SCAN);
      matchs ("simd", gfc_match_omp_simd, ST_OMP_SIMD);
      break;
    case 'n':
      matcho ("nothing", gfc_match_omp_nothing, ST_NONE);
      break;
    }

  pure_ok = false;
  if (flag_openmp && gfc_pure (NULL))
    {
      gfc_error_now ("OpenMP directive at %C is not pure and thus may not "
		     "appear in a PURE procedure");
      gfc_error_recovery ();
      return ST_NONE;
    }

  /* match is for directives that should be recognized only if
     -fopenmp, matchs for directives that should be recognized
     if either -fopenmp or -fopenmp-simd.  */
  switch (c)
    {
    case 'a':
      if (in_exec_part)
	matcho ("allocate", gfc_match_omp_allocate, ST_OMP_ALLOCATE_EXEC);
      else
	matcho ("allocate", gfc_match_omp_allocate, ST_OMP_ALLOCATE);
      matcho ("allocators", gfc_match_omp_allocators, ST_OMP_ALLOCATORS);
      matcho ("atomic", gfc_match_omp_atomic, ST_OMP_ATOMIC);
      break;
    case 'b':
      matcho ("barrier", gfc_match_omp_barrier, ST_OMP_BARRIER);
      break;
    case 'c':
      matcho ("cancellation% point", gfc_match_omp_cancellation_point,
	      ST_OMP_CANCELLATION_POINT);
      matcho ("cancel", gfc_match_omp_cancel, ST_OMP_CANCEL);
      matcho ("critical", gfc_match_omp_critical, ST_OMP_CRITICAL);
      break;
    case 'd':
      matcho ("depobj", gfc_match_omp_depobj, ST_OMP_DEPOBJ);
      matchs ("distribute parallel do simd",
	      gfc_match_omp_distribute_parallel_do_simd,
	      ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD);
      matcho ("distribute parallel do", gfc_match_omp_distribute_parallel_do,
	      ST_OMP_DISTRIBUTE_PARALLEL_DO);
      matchs ("distribute simd", gfc_match_omp_distribute_simd,
	      ST_OMP_DISTRIBUTE_SIMD);
      matcho ("distribute", gfc_match_omp_distribute, ST_OMP_DISTRIBUTE);
      matchs ("do simd", gfc_match_omp_do_simd, ST_OMP_DO_SIMD);
      matcho ("do", gfc_match_omp_do, ST_OMP_DO);
      break;
    case 'e':
      matcho ("end allocators", gfc_match_omp_eos_error, ST_OMP_END_ALLOCATORS);
      matcho ("end atomic", gfc_match_omp_eos_error, ST_OMP_END_ATOMIC);
      matcho ("end critical", gfc_match_omp_end_critical, ST_OMP_END_CRITICAL);
      matchs ("end distribute parallel do simd", gfc_match_omp_eos_error,
	      ST_OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD);
      matcho ("end distribute parallel do", gfc_match_omp_eos_error,
	      ST_OMP_END_DISTRIBUTE_PARALLEL_DO);
      matchs ("end distribute simd", gfc_match_omp_eos_error,
	      ST_OMP_END_DISTRIBUTE_SIMD);
      matcho ("end distribute", gfc_match_omp_eos_error, ST_OMP_END_DISTRIBUTE);
      matchs ("end do simd", gfc_match_omp_end_nowait, ST_OMP_END_DO_SIMD);
      matcho ("end do", gfc_match_omp_end_nowait, ST_OMP_END_DO);
      matchs ("end loop", gfc_match_omp_eos_error, ST_OMP_END_LOOP);
      matcho ("end masked taskloop simd", gfc_match_omp_eos_error,
	      ST_OMP_END_MASKED_TASKLOOP_SIMD);
      matcho ("end masked taskloop", gfc_match_omp_eos_error,
	      ST_OMP_END_MASKED_TASKLOOP);
      matcho ("end masked", gfc_match_omp_eos_error, ST_OMP_END_MASKED);
      matcho ("end master taskloop simd", gfc_match_omp_eos_error,
	      ST_OMP_END_MASTER_TASKLOOP_SIMD);
      matcho ("end master taskloop", gfc_match_omp_eos_error,
	      ST_OMP_END_MASTER_TASKLOOP);
      matcho ("end master", gfc_match_omp_eos_error, ST_OMP_END_MASTER);
      matchs ("end ordered", gfc_match_omp_eos_error, ST_OMP_END_ORDERED);
      matchs ("end parallel do simd", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_DO_SIMD);
      matcho ("end parallel do", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_DO);
      matcho ("end parallel loop", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_LOOP);
      matcho ("end parallel masked taskloop simd", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_MASKED_TASKLOOP_SIMD);
      matcho ("end parallel masked taskloop", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_MASKED_TASKLOOP);
      matcho ("end parallel masked", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_MASKED);
      matcho ("end parallel master taskloop simd", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_MASTER_TASKLOOP_SIMD);
      matcho ("end parallel master taskloop", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_MASTER_TASKLOOP);
      matcho ("end parallel master", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_MASTER);
      matcho ("end parallel sections", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_SECTIONS);
      matcho ("end parallel workshare", gfc_match_omp_eos_error,
	      ST_OMP_END_PARALLEL_WORKSHARE);
      matcho ("end parallel", gfc_match_omp_eos_error, ST_OMP_END_PARALLEL);
      matcho ("end scope", gfc_match_omp_end_nowait, ST_OMP_END_SCOPE);
      matcho ("end sections", gfc_match_omp_end_nowait, ST_OMP_END_SECTIONS);
      matcho ("end single", gfc_match_omp_end_single, ST_OMP_END_SINGLE);
      matcho ("end target data", gfc_match_omp_eos_error, ST_OMP_END_TARGET_DATA);
      matchs ("end target parallel do simd", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_PARALLEL_DO_SIMD);
      matcho ("end target parallel do", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_PARALLEL_DO);
      matcho ("end target parallel loop", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_PARALLEL_LOOP);
      matcho ("end target parallel", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_PARALLEL);
      matchs ("end target simd", gfc_match_omp_end_nowait, ST_OMP_END_TARGET_SIMD);
      matchs ("end target teams distribute parallel do simd",
	      gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD);
      matcho ("end target teams distribute parallel do", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO);
      matchs ("end target teams distribute simd", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD);
      matcho ("end target teams distribute", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_TEAMS_DISTRIBUTE);
      matcho ("end target teams loop", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_TEAMS_LOOP);
      matcho ("end target teams", gfc_match_omp_end_nowait,
	      ST_OMP_END_TARGET_TEAMS);
      matcho ("end target", gfc_match_omp_end_nowait, ST_OMP_END_TARGET);
      matcho ("end taskgroup", gfc_match_omp_eos_error, ST_OMP_END_TASKGROUP);
      matchs ("end taskloop simd", gfc_match_omp_eos_error,
	      ST_OMP_END_TASKLOOP_SIMD);
      matcho ("end taskloop", gfc_match_omp_eos_error, ST_OMP_END_TASKLOOP);
      matcho ("end task", gfc_match_omp_eos_error, ST_OMP_END_TASK);
      matchs ("end teams distribute parallel do simd", gfc_match_omp_eos_error,
	      ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD);
      matcho ("end teams distribute parallel do", gfc_match_omp_eos_error,
	      ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO);
      matchs ("end teams distribute simd", gfc_match_omp_eos_error,
	      ST_OMP_END_TEAMS_DISTRIBUTE_SIMD);
      matcho ("end teams distribute", gfc_match_omp_eos_error,
	      ST_OMP_END_TEAMS_DISTRIBUTE);
      matcho ("end teams loop", gfc_match_omp_eos_error, ST_OMP_END_TEAMS_LOOP);
      matcho ("end teams", gfc_match_omp_eos_error, ST_OMP_END_TEAMS);
      matcho ("end workshare", gfc_match_omp_end_nowait,
	      ST_OMP_END_WORKSHARE);
      break;
    case 'f':
      matcho ("flush", gfc_match_omp_flush, ST_OMP_FLUSH);
      break;
    case 'm':
      matcho ("masked taskloop simd", gfc_match_omp_masked_taskloop_simd,
	      ST_OMP_MASKED_TASKLOOP_SIMD);
      matcho ("masked taskloop", gfc_match_omp_masked_taskloop,
	      ST_OMP_MASKED_TASKLOOP);
      matcho ("masked", gfc_match_omp_masked, ST_OMP_MASKED);
      matcho ("master taskloop simd", gfc_match_omp_master_taskloop_simd,
	      ST_OMP_MASTER_TASKLOOP_SIMD);
      matcho ("master taskloop", gfc_match_omp_master_taskloop,
	      ST_OMP_MASTER_TASKLOOP);
      matcho ("master", gfc_match_omp_master, ST_OMP_MASTER);
      break;
    case 'n':
      matcho ("nothing", gfc_match_omp_nothing, ST_NONE);
      break;
    case 'l':
      matchs ("loop", gfc_match_omp_loop, ST_OMP_LOOP);
      break;
    case 'o':
      if (gfc_match ("ordered depend (") == MATCH_YES
	  || gfc_match ("ordered doacross (") == MATCH_YES)
	{
	  gfc_current_locus = old_locus;
	  if (!flag_openmp)
	    break;
	  matcho ("ordered", gfc_match_omp_ordered_depend,
		  ST_OMP_ORDERED_DEPEND);
	}
      else
	matchs ("ordered", gfc_match_omp_ordered, ST_OMP_ORDERED);
      break;
    case 'p':
      matchs ("parallel do simd", gfc_match_omp_parallel_do_simd,
	      ST_OMP_PARALLEL_DO_SIMD);
      matcho ("parallel do", gfc_match_omp_parallel_do, ST_OMP_PARALLEL_DO);
      matcho ("parallel loop", gfc_match_omp_parallel_loop,
	      ST_OMP_PARALLEL_LOOP);
      matcho ("parallel masked taskloop simd",
	      gfc_match_omp_parallel_masked_taskloop_simd,
	      ST_OMP_PARALLEL_MASKED_TASKLOOP_SIMD);
      matcho ("parallel masked taskloop",
	      gfc_match_omp_parallel_masked_taskloop,
	      ST_OMP_PARALLEL_MASKED_TASKLOOP);
      matcho ("parallel masked", gfc_match_omp_parallel_masked,
	      ST_OMP_PARALLEL_MASKED);
      matcho ("parallel master taskloop simd",
	      gfc_match_omp_parallel_master_taskloop_simd,
	      ST_OMP_PARALLEL_MASTER_TASKLOOP_SIMD);
      matcho ("parallel master taskloop",
	      gfc_match_omp_parallel_master_taskloop,
	      ST_OMP_PARALLEL_MASTER_TASKLOOP);
      matcho ("parallel master", gfc_match_omp_parallel_master,
	      ST_OMP_PARALLEL_MASTER);
      matcho ("parallel sections", gfc_match_omp_parallel_sections,
	      ST_OMP_PARALLEL_SECTIONS);
      matcho ("parallel workshare", gfc_match_omp_parallel_workshare,
	      ST_OMP_PARALLEL_WORKSHARE);
      matcho ("parallel", gfc_match_omp_parallel, ST_OMP_PARALLEL);
      break;
    case 'r':
      matcho ("requires", gfc_match_omp_requires, ST_OMP_REQUIRES);
      break;
    case 's':
      matcho ("scope", gfc_match_omp_scope, ST_OMP_SCOPE);
      matcho ("sections", gfc_match_omp_sections, ST_OMP_SECTIONS);
      matcho ("section", gfc_match_omp_eos_error, ST_OMP_SECTION);
      matcho ("single", gfc_match_omp_single, ST_OMP_SINGLE);
      break;
    case 't':
      matcho ("target data", gfc_match_omp_target_data, ST_OMP_TARGET_DATA);
      matcho ("target enter data", gfc_match_omp_target_enter_data,
	      ST_OMP_TARGET_ENTER_DATA);
      matcho ("target exit data", gfc_match_omp_target_exit_data,
	      ST_OMP_TARGET_EXIT_DATA);
      matchs ("target parallel do simd", gfc_match_omp_target_parallel_do_simd,
	      ST_OMP_TARGET_PARALLEL_DO_SIMD);
      matcho ("target parallel do", gfc_match_omp_target_parallel_do,
	      ST_OMP_TARGET_PARALLEL_DO);
      matcho ("target parallel loop", gfc_match_omp_target_parallel_loop,
	      ST_OMP_TARGET_PARALLEL_LOOP);
      matcho ("target parallel", gfc_match_omp_target_parallel,
	      ST_OMP_TARGET_PARALLEL);
      matchs ("target simd", gfc_match_omp_target_simd, ST_OMP_TARGET_SIMD);
      matchs ("target teams distribute parallel do simd",
	      gfc_match_omp_target_teams_distribute_parallel_do_simd,
	      ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD);
      matcho ("target teams distribute parallel do",
	      gfc_match_omp_target_teams_distribute_parallel_do,
	      ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO);
      matchs ("target teams distribute simd",
	      gfc_match_omp_target_teams_distribute_simd,
	      ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD);
      matcho ("target teams distribute", gfc_match_omp_target_teams_distribute,
	      ST_OMP_TARGET_TEAMS_DISTRIBUTE);
      matcho ("target teams loop", gfc_match_omp_target_teams_loop,
	      ST_OMP_TARGET_TEAMS_LOOP);
      matcho ("target teams", gfc_match_omp_target_teams, ST_OMP_TARGET_TEAMS);
      matcho ("target update", gfc_match_omp_target_update,
	      ST_OMP_TARGET_UPDATE);
      matcho ("target", gfc_match_omp_target, ST_OMP_TARGET);
      matcho ("taskgroup", gfc_match_omp_taskgroup, ST_OMP_TASKGROUP);
      matchs ("taskloop simd", gfc_match_omp_taskloop_simd,
	      ST_OMP_TASKLOOP_SIMD);
      matcho ("taskloop", gfc_match_omp_taskloop, ST_OMP_TASKLOOP);
      matcho ("taskwait", gfc_match_omp_taskwait, ST_OMP_TASKWAIT);
      matcho ("taskyield", gfc_match_omp_taskyield, ST_OMP_TASKYIELD);
      matcho ("task", gfc_match_omp_task, ST_OMP_TASK);
      matchs ("teams distribute parallel do simd",
	      gfc_match_omp_teams_distribute_parallel_do_simd,
	      ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD);
      matcho ("teams distribute parallel do",
	      gfc_match_omp_teams_distribute_parallel_do,
	      ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO);
      matchs ("teams distribute simd", gfc_match_omp_teams_distribute_simd,
	      ST_OMP_TEAMS_DISTRIBUTE_SIMD);
      matcho ("teams distribute", gfc_match_omp_teams_distribute,
	      ST_OMP_TEAMS_DISTRIBUTE);
      matcho ("teams loop", gfc_match_omp_teams_loop, ST_OMP_TEAMS_LOOP);
      matcho ("teams", gfc_match_omp_teams, ST_OMP_TEAMS);
      matchdo ("threadprivate", gfc_match_omp_threadprivate,
	       ST_OMP_THREADPRIVATE);
      break;
    case 'w':
      matcho ("workshare", gfc_match_omp_workshare, ST_OMP_WORKSHARE);
      break;
    }

  /* All else has failed, so give up.  See if any of the matchers has
     stored an error message of some sort.  Don't error out if
     not -fopenmp and simd_matched is false, i.e. if a directive other
     than one marked with match has been seen.  */

 error_handling:
  if (flag_openmp || simd_matched)
    {
      if (!gfc_error_check ())
	gfc_error_now ("Unclassifiable OpenMP directive at %C");
    }

  reject_statement ();

  gfc_error_recovery ();

  return ST_NONE;

 finish:
  if (ret == ST_OMP_ERROR && new_st.ext.omp_clauses->at == OMP_AT_EXECUTION)
    {
      gfc_unset_implicit_pure (NULL);

      if (gfc_pure (NULL))
	{
	  gfc_error_now ("OpenMP ERROR directive at %L with %<at(execution)%> "
			 "clause in a PURE procedure", &old_locus);
	  reject_statement ();
	  gfc_error_recovery ();
	  return ST_NONE;
	}
    }
  if (!pure_ok)
    {
      gfc_unset_implicit_pure (NULL);

      if (!flag_openmp && gfc_pure (NULL))
	{
	  gfc_error_now ("OpenMP directive at %C is not pure and thus may not "
			 "appear in a PURE procedure");
	  reject_statement ();
	  gfc_error_recovery ();
	  return ST_NONE;
	}
    }
  if (ret == ST_OMP_ALLOCATE && !check_omp_allocate_stmt (&old_locus))
    goto error_handling;

  switch (ret)
    {
    /* Set omp_target_seen; exclude ST_OMP_DECLARE_TARGET.
       FIXME: Get clarification, cf. OpenMP Spec Issue #3240.  */
    case ST_OMP_TARGET:
    case ST_OMP_TARGET_DATA:
    case ST_OMP_TARGET_ENTER_DATA:
    case ST_OMP_TARGET_EXIT_DATA:
    case ST_OMP_TARGET_TEAMS:
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE:
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case ST_OMP_TARGET_TEAMS_LOOP:
    case ST_OMP_TARGET_PARALLEL:
    case ST_OMP_TARGET_PARALLEL_DO:
    case ST_OMP_TARGET_PARALLEL_DO_SIMD:
    case ST_OMP_TARGET_PARALLEL_LOOP:
    case ST_OMP_TARGET_SIMD:
    case ST_OMP_TARGET_UPDATE:
      {
	gfc_namespace *prog_unit = gfc_current_ns;
	while (prog_unit->parent)
	  {
	    if (gfc_state_stack->previous
		&& gfc_state_stack->previous->state == COMP_INTERFACE)
	      break;
	    prog_unit = prog_unit->parent;
	  }
	  prog_unit->omp_target_seen = true;
	break;
      }
    case ST_OMP_ALLOCATE_EXEC:
    case ST_OMP_ALLOCATORS:
    case ST_OMP_TEAMS:
    case ST_OMP_TEAMS_DISTRIBUTE:
    case ST_OMP_TEAMS_DISTRIBUTE_SIMD:
    case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case ST_OMP_TEAMS_LOOP:
      for (gfc_state_data *stk = gfc_state_stack->previous; stk;
	   stk = stk->previous)
	if (stk && stk->tail)
	  switch (stk->tail->op)
	    {
	    case EXEC_OMP_TARGET:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_TEAMS_LOOP:
	    case EXEC_OMP_TARGET_PARALLEL:
	    case EXEC_OMP_TARGET_PARALLEL_DO:
	    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_PARALLEL_LOOP:
	    case EXEC_OMP_TARGET_SIMD:
	      if (ret == ST_OMP_ALLOCATE_EXEC || ret == ST_OMP_ALLOCATORS)
		new_st.ext.omp_clauses->contained_in_target_construct = 1;
	      else
		stk->tail->ext.omp_clauses->contains_teams_construct = 1;
	      break;
	    default:
	      break;
	    }
      break;
    case ST_OMP_ERROR:
      if (new_st.ext.omp_clauses->at != OMP_AT_EXECUTION)
	return ST_NONE;
    default:
      break;
    }
  return ret;

 do_spec_only:
  reject_statement ();
  gfc_clear_error ();
  gfc_buffer_error (false);
  gfc_current_locus = old_locus;
  return ST_GET_FCN_CHARACTERISTICS;
}

static gfc_statement
decode_gcc_attribute (void)
{
  locus old_locus;

  gfc_enforce_clean_symbol_state ();

  gfc_clear_error ();	/* Clear any pending errors.  */
  gfc_clear_warning ();	/* Clear any pending warnings.  */
  old_locus = gfc_current_locus;

  match ("attributes", gfc_match_gcc_attributes, ST_ATTR_DECL);
  match ("unroll", gfc_match_gcc_unroll, ST_NONE);
  match ("builtin", gfc_match_gcc_builtin, ST_NONE);
  match ("ivdep", gfc_match_gcc_ivdep, ST_NONE);
  match ("vector", gfc_match_gcc_vector, ST_NONE);
  match ("novector", gfc_match_gcc_novector, ST_NONE);

  /* All else has failed, so give up.  See if any of the matchers has
     stored an error message of some sort.  */

  if (!gfc_error_check ())
    {
      if (pedantic)
	gfc_error_now ("Unclassifiable GCC directive at %C");
      else
	gfc_warning_now (0, "Unclassifiable GCC directive at %C, ignored");
    }

  reject_statement ();

  gfc_error_recovery ();

  return ST_NONE;
}

#undef match

/* Assert next length characters to be equal to token in free form.  */

static void
verify_token_free (const char* token, int length, bool last_was_use_stmt)
{
  int i;
  char c;

  c = gfc_next_ascii_char ();
  for (i = 0; i < length; i++, c = gfc_next_ascii_char ())
    gcc_assert (c == token[i]);

  gcc_assert (gfc_is_whitespace(c));
  gfc_gobble_whitespace ();
  if (last_was_use_stmt)
    use_modules ();
}

/* Get the next statement in free form source.  */

static gfc_statement
next_free (void)
{
  match m;
  int i, cnt, at_bol;
  char c;

  at_bol = gfc_at_bol ();
  gfc_gobble_whitespace ();

  c = gfc_peek_ascii_char ();

  if (ISDIGIT (c))
    {
      char d;

      /* Found a statement label?  */
      m = gfc_match_st_label (&gfc_statement_label);

      d = gfc_peek_ascii_char ();
      if (m != MATCH_YES || !gfc_is_whitespace (d))
	{
	  gfc_match_small_literal_int (&i, &cnt);

	  if (cnt > 5)
	    gfc_error_now ("Too many digits in statement label at %C");

	  if (i == 0)
	    gfc_error_now ("Zero is not a valid statement label at %C");

	  do
	    c = gfc_next_ascii_char ();
	  while (ISDIGIT(c));

	  if (!gfc_is_whitespace (c))
	    gfc_error_now ("Non-numeric character in statement label at %C");

	  return ST_NONE;
	}
      else
	{
	  label_locus = gfc_current_locus;

	  gfc_gobble_whitespace ();

	  if (at_bol && gfc_peek_ascii_char () == ';')
	    {
	      gfc_error_now ("Semicolon at %C needs to be preceded by "
			     "statement");
	      gfc_next_ascii_char (); /* Eat up the semicolon.  */
	      return ST_NONE;
	    }

	  if (gfc_match_eos () == MATCH_YES)
	    gfc_error_now ("Statement label without statement at %L",
			   &label_locus);
	}
    }
  else if (c == '!')
    {
      /* Comments have already been skipped by the time we get here,
	 except for GCC attributes and OpenMP/OpenACC directives.  */

      gfc_next_ascii_char (); /* Eat up the exclamation sign.  */
      c = gfc_peek_ascii_char ();

      if (c == 'g')
	{
	  int i;

	  c = gfc_next_ascii_char ();
	  for (i = 0; i < 4; i++, c = gfc_next_ascii_char ())
	    gcc_assert (c == "gcc$"[i]);

	  gfc_gobble_whitespace ();
	  return decode_gcc_attribute ();

	}
      else if (c == '$')
	{
	  /* Since both OpenMP and OpenACC directives starts with
	     !$ character sequence, we must check all flags combinations */
	  if ((flag_openmp || flag_openmp_simd)
	      && !flag_openacc)
	    {
	      verify_token_free ("$omp", 4, last_was_use_stmt);
	      return decode_omp_directive ();
	    }
	  else if ((flag_openmp || flag_openmp_simd)
		   && flag_openacc)
	    {
	      gfc_next_ascii_char (); /* Eat up dollar character */
	      c = gfc_peek_ascii_char ();

	      if (c == 'o')
		{
		  verify_token_free ("omp", 3, last_was_use_stmt);
		  return decode_omp_directive ();
		}
	      else if (c == 'a')
		{
		  verify_token_free ("acc", 3, last_was_use_stmt);
		  return decode_oacc_directive ();
		}
	    }
	  else if (flag_openacc)
	    {
	      verify_token_free ("$acc", 4, last_was_use_stmt);
	      return decode_oacc_directive ();
	    }
	}
      gcc_unreachable ();
    }

  if (at_bol && c == ';')
    {
      if (!(gfc_option.allow_std & GFC_STD_F2008))
	gfc_error_now ("Fortran 2008: Semicolon at %C without preceding "
		       "statement");
      gfc_next_ascii_char (); /* Eat up the semicolon.  */
      return ST_NONE;
    }

  return decode_statement ();
}

/* Assert next length characters to be equal to token in fixed form.  */

static bool
verify_token_fixed (const char *token, int length, bool last_was_use_stmt)
{
  int i;
  char c = gfc_next_char_literal (NONSTRING);

  for (i = 0; i < length; i++, c = gfc_next_char_literal (NONSTRING))
    gcc_assert ((char) gfc_wide_tolower (c) == token[i]);

  if (c != ' ' && c != '0')
    {
      gfc_buffer_error (false);
      gfc_error ("Bad continuation line at %C");
      return false;
    }
  if (last_was_use_stmt)
    use_modules ();

  return true;
}

/* Get the next statement in fixed-form source.  */

static gfc_statement
next_fixed (void)
{
  int label, digit_flag, i;
  locus loc;
  gfc_char_t c;

  if (!gfc_at_bol ())
    return decode_statement ();

  /* Skip past the current label field, parsing a statement label if
     one is there.  This is a weird number parser, since the number is
     contained within five columns and can have any kind of embedded
     spaces.  We also check for characters that make the rest of the
     line a comment.  */

  label = 0;
  digit_flag = 0;

  for (i = 0; i < 5; i++)
    {
      c = gfc_next_char_literal (NONSTRING);

      switch (c)
	{
	case ' ':
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  label = label * 10 + ((unsigned char) c - '0');
	  label_locus = gfc_current_locus;
	  digit_flag = 1;
	  break;

	  /* Comments have already been skipped by the time we get
	     here, except for GCC attributes and OpenMP directives.  */

	case '*':
	  c = gfc_next_char_literal (NONSTRING);

	  if (TOLOWER (c) == 'g')
	    {
	      for (i = 0; i < 4; i++, c = gfc_next_char_literal (NONSTRING))
		gcc_assert (TOLOWER (c) == "gcc$"[i]);

	      return decode_gcc_attribute ();
	    }
	  else if (c == '$')
	    {
	      if ((flag_openmp || flag_openmp_simd)
		  && !flag_openacc)
		{
		  if (!verify_token_fixed ("omp", 3, last_was_use_stmt))
		    return ST_NONE;
		  return decode_omp_directive ();
		}
	      else if ((flag_openmp || flag_openmp_simd)
		       && flag_openacc)
		{
		  c = gfc_next_char_literal(NONSTRING);
		  if (c == 'o' || c == 'O')
		    {
		      if (!verify_token_fixed ("mp", 2, last_was_use_stmt))
			return ST_NONE;
		      return decode_omp_directive ();
		    }
		  else if (c == 'a' || c == 'A')
		    {
		      if (!verify_token_fixed ("cc", 2, last_was_use_stmt))
			return ST_NONE;
		      return decode_oacc_directive ();
		    }
		}
	      else if (flag_openacc)
		{
		  if (!verify_token_fixed ("acc", 3, last_was_use_stmt))
		    return ST_NONE;
		  return decode_oacc_directive ();
		}
	    }
	  gcc_fallthrough ();

	  /* Comments have already been skipped by the time we get
	     here so don't bother checking for them.  */

	default:
	  gfc_buffer_error (false);
	  gfc_error ("Non-numeric character in statement label at %C");
	  return ST_NONE;
	}
    }

  if (digit_flag)
    {
      if (label == 0)
	gfc_warning_now (0, "Zero is not a valid statement label at %C");
      else
	{
	  /* We've found a valid statement label.  */
	  gfc_statement_label = gfc_get_st_label (label);
	}
    }

  /* Since this line starts a statement, it cannot be a continuation
     of a previous statement.  If we see something here besides a
     space or zero, it must be a bad continuation line.  */

  c = gfc_next_char_literal (NONSTRING);
  if (c == '\n')
    goto blank_line;

  if (c != ' ' && c != '0')
    {
      gfc_buffer_error (false);
      gfc_error ("Bad continuation line at %C");
      return ST_NONE;
    }

  /* Now that we've taken care of the statement label columns, we have
     to make sure that the first nonblank character is not a '!'.  If
     it is, the rest of the line is a comment.  */

  do
    {
      loc = gfc_current_locus;
      c = gfc_next_char_literal (NONSTRING);
    }
  while (gfc_is_whitespace (c));

  if (c == '!')
    goto blank_line;
  gfc_current_locus = loc;

  if (c == ';')
    {
      if (digit_flag)
	gfc_error_now ("Semicolon at %C needs to be preceded by statement");
      else if (!(gfc_option.allow_std & GFC_STD_F2008))
	gfc_error_now ("Fortran 2008: Semicolon at %C without preceding "
		       "statement");
      return ST_NONE;
    }

  if (gfc_match_eos () == MATCH_YES)
    goto blank_line;

  /* At this point, we've got a nonblank statement to parse.  */
  return decode_statement ();

blank_line:
  if (digit_flag)
    gfc_error_now ("Statement label without statement at %L", &label_locus);

  gfc_current_locus.lb->truncated = 0;
  gfc_advance_line ();
  return ST_NONE;
}


/* Return the next non-ST_NONE statement to the caller.  We also worry
   about including files and the ends of include files at this stage.  */

static gfc_statement
next_statement (void)
{
  gfc_statement st;
  locus old_locus;

  gfc_enforce_clean_symbol_state ();
  gfc_save_module_list ();

  gfc_new_block = NULL;

  gfc_current_ns->old_equiv = gfc_current_ns->equiv;
  gfc_current_ns->old_data = gfc_current_ns->data;
  for (;;)
    {
      gfc_statement_label = NULL;
      gfc_buffer_error (true);

      if (gfc_at_eol ())
	gfc_advance_line ();

      gfc_skip_comments ();

      if (gfc_at_end ())
	{
	  st = ST_NONE;
	  break;
	}

      if (gfc_define_undef_line ())
	continue;

      old_locus = gfc_current_locus;

      st = (gfc_current_form == FORM_FIXED) ? next_fixed () : next_free ();

      if (st != ST_NONE)
	break;
    }

  gfc_buffer_error (false);

  if (st == ST_GET_FCN_CHARACTERISTICS)
    {
      if (gfc_statement_label != NULL)
	{
	  gfc_free_st_label (gfc_statement_label);
	  gfc_statement_label = NULL;
	}
      gfc_current_locus = old_locus;
    }

  if (st != ST_NONE)
    check_statement_label (st);

  return st;
}


/****************************** Parser ***********************************/

/* The parser subroutines are of type 'try' that fail if the file ends
   unexpectedly.  */

/* Macros that expand to case-labels for various classes of
   statements.  Start with executable statements that directly do
   things.  */

#define case_executable case ST_ALLOCATE: case ST_BACKSPACE: case ST_CALL: \
  case ST_CLOSE: case ST_CONTINUE: case ST_DEALLOCATE: case ST_END_FILE: \
  case ST_GOTO: case ST_INQUIRE: case ST_NULLIFY: case ST_OPEN: \
  case ST_READ: case ST_RETURN: case ST_REWIND: case ST_SIMPLE_IF: \
  case ST_PAUSE: case ST_STOP: case ST_WAIT: case ST_WRITE: \
  case ST_POINTER_ASSIGNMENT: case ST_EXIT: case ST_CYCLE: \
  case ST_ASSIGNMENT: case ST_ARITHMETIC_IF: case ST_WHERE: case ST_FORALL: \
  case ST_LABEL_ASSIGNMENT: case ST_FLUSH: case ST_OMP_FLUSH: \
  case ST_OMP_BARRIER: case ST_OMP_TASKWAIT: case ST_OMP_TASKYIELD: \
  case ST_OMP_CANCEL: case ST_OMP_CANCELLATION_POINT: case ST_OMP_DEPOBJ: \
  case ST_OMP_TARGET_UPDATE: case ST_OMP_TARGET_ENTER_DATA: \
  case ST_OMP_TARGET_EXIT_DATA: case ST_OMP_ORDERED_DEPEND: case ST_OMP_ERROR: \
  case ST_ERROR_STOP: case ST_OMP_SCAN: case ST_SYNC_ALL: \
  case ST_SYNC_IMAGES: case ST_SYNC_MEMORY: case ST_LOCK: case ST_UNLOCK: \
  case ST_FORM_TEAM: case ST_CHANGE_TEAM: \
  case ST_END_TEAM: case ST_SYNC_TEAM: \
  case ST_EVENT_POST: case ST_EVENT_WAIT: case ST_FAIL_IMAGE: \
  case ST_OACC_UPDATE: case ST_OACC_WAIT: case ST_OACC_CACHE: \
  case ST_OACC_ENTER_DATA: case ST_OACC_EXIT_DATA

/* Statements that mark other executable statements.  */

#define case_exec_markers case ST_DO: case ST_FORALL_BLOCK: \
  case ST_IF_BLOCK: case ST_BLOCK: case ST_ASSOCIATE: \
  case ST_WHERE_BLOCK: case ST_SELECT_CASE: case ST_SELECT_TYPE: \
  case ST_SELECT_RANK: case ST_OMP_PARALLEL: case ST_OMP_PARALLEL_MASKED: \
  case ST_OMP_PARALLEL_MASKED_TASKLOOP: \
  case ST_OMP_PARALLEL_MASKED_TASKLOOP_SIMD: case ST_OMP_PARALLEL_MASTER: \
  case ST_OMP_PARALLEL_MASTER_TASKLOOP: \
  case ST_OMP_PARALLEL_MASTER_TASKLOOP_SIMD: \
  case ST_OMP_PARALLEL_SECTIONS: case ST_OMP_SECTIONS: case ST_OMP_ORDERED: \
  case ST_OMP_CRITICAL: case ST_OMP_MASKED: case ST_OMP_MASKED_TASKLOOP: \
  case ST_OMP_MASKED_TASKLOOP_SIMD: \
  case ST_OMP_MASTER: case ST_OMP_MASTER_TASKLOOP: \
  case ST_OMP_MASTER_TASKLOOP_SIMD: case ST_OMP_SCOPE: case ST_OMP_SINGLE: \
  case ST_OMP_DO: case ST_OMP_PARALLEL_DO: case ST_OMP_ATOMIC: \
  case ST_OMP_WORKSHARE: case ST_OMP_PARALLEL_WORKSHARE: \
  case ST_OMP_TASK: case ST_OMP_TASKGROUP: case ST_OMP_SIMD: \
  case ST_OMP_DO_SIMD: case ST_OMP_PARALLEL_DO_SIMD: case ST_OMP_TARGET: \
  case ST_OMP_TARGET_DATA: case ST_OMP_TARGET_TEAMS: \
  case ST_OMP_TARGET_TEAMS_DISTRIBUTE: \
  case ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD: \
  case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO: \
  case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD: \
  case ST_OMP_TEAMS: case ST_OMP_TEAMS_DISTRIBUTE: \
  case ST_OMP_TEAMS_DISTRIBUTE_SIMD: \
  case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO: \
  case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD: case ST_OMP_DISTRIBUTE: \
  case ST_OMP_DISTRIBUTE_SIMD: case ST_OMP_DISTRIBUTE_PARALLEL_DO: \
  case ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD: case ST_OMP_TARGET_PARALLEL: \
  case ST_OMP_TARGET_PARALLEL_DO: case ST_OMP_TARGET_PARALLEL_DO_SIMD: \
  case ST_OMP_TARGET_SIMD: case ST_OMP_TASKLOOP: case ST_OMP_TASKLOOP_SIMD: \
  case ST_OMP_LOOP: case ST_OMP_PARALLEL_LOOP: case ST_OMP_TEAMS_LOOP: \
  case ST_OMP_TARGET_PARALLEL_LOOP: case ST_OMP_TARGET_TEAMS_LOOP: \
  case ST_OMP_ALLOCATE_EXEC: case ST_OMP_ALLOCATORS: case ST_OMP_ASSUME: \
  case ST_CRITICAL: \
  case ST_OACC_PARALLEL_LOOP: case ST_OACC_PARALLEL: case ST_OACC_KERNELS: \
  case ST_OACC_DATA: case ST_OACC_HOST_DATA: case ST_OACC_LOOP: \
  case ST_OACC_KERNELS_LOOP: case ST_OACC_SERIAL_LOOP: case ST_OACC_SERIAL: \
  case ST_OACC_ATOMIC

/* Declaration statements */

#define case_decl case ST_ATTR_DECL: case ST_COMMON: case ST_DATA_DECL: \
  case ST_EQUIVALENCE: case ST_NAMELIST: case ST_STATEMENT_FUNCTION: \
  case ST_TYPE: case ST_INTERFACE: case ST_PROCEDURE

/* OpenMP and OpenACC declaration statements, which may appear anywhere in
   the specification part.  */

#define case_omp_decl case ST_OMP_THREADPRIVATE: case ST_OMP_DECLARE_SIMD: \
  case ST_OMP_DECLARE_TARGET: case ST_OMP_DECLARE_REDUCTION: \
  case ST_OMP_DECLARE_VARIANT: case ST_OMP_ALLOCATE: case ST_OMP_ASSUMES: \
  case ST_OMP_REQUIRES: case ST_OACC_ROUTINE: case ST_OACC_DECLARE

/* Block end statements.  Errors associated with interchanging these
   are detected in gfc_match_end().  */

#define case_end case ST_END_BLOCK_DATA: case ST_END_FUNCTION: \
		 case ST_END_PROGRAM: case ST_END_SUBROUTINE: \
		 case ST_END_BLOCK: case ST_END_ASSOCIATE


/* Push a new state onto the stack.  */

static void
push_state (gfc_state_data *p, gfc_compile_state new_state, gfc_symbol *sym)
{
  p->state = new_state;
  p->previous = gfc_state_stack;
  p->sym = sym;
  p->head = p->tail = NULL;
  p->do_variable = NULL;
  if (p->state != COMP_DO && p->state != COMP_DO_CONCURRENT)
    p->ext.oacc_declare_clauses = NULL;

  /* If this the state of a construct like BLOCK, DO or IF, the corresponding
     construct statement was accepted right before pushing the state.  Thus,
     the construct's gfc_code is available as tail of the parent state.  */
  gcc_assert (gfc_state_stack);
  p->construct = gfc_state_stack->tail;

  gfc_state_stack = p;
}


/* Pop the current state.  */
static void
pop_state (void)
{
  gfc_state_stack = gfc_state_stack->previous;
}


/* Try to find the given state in the state stack.  */

bool
gfc_find_state (gfc_compile_state state)
{
  gfc_state_data *p;

  for (p = gfc_state_stack; p; p = p->previous)
    if (p->state == state)
      break;

  return (p == NULL) ? false : true;
}


/* Starts a new level in the statement list.  */

static gfc_code *
new_level (gfc_code *q)
{
  gfc_code *p;

  p = q->block = gfc_get_code (EXEC_NOP);

  gfc_state_stack->head = gfc_state_stack->tail = p;

  return p;
}


/* Add the current new_st code structure and adds it to the current
   program unit.  As a side-effect, it zeroes the new_st.  */

static gfc_code *
add_statement (void)
{
  gfc_code *p;

  p = XCNEW (gfc_code);
  *p = new_st;

  p->loc = gfc_current_locus;

  if (gfc_state_stack->head == NULL)
    gfc_state_stack->head = p;
  else
    gfc_state_stack->tail->next = p;

  while (p->next != NULL)
    p = p->next;

  gfc_state_stack->tail = p;

  gfc_clear_new_st ();

  return p;
}


/* Frees everything associated with the current statement.  */

static void
undo_new_statement (void)
{
  gfc_free_statements (new_st.block);
  gfc_free_statements (new_st.next);
  gfc_free_statement (&new_st);
  gfc_clear_new_st ();
}


/* If the current statement has a statement label, make sure that it
   is allowed to, or should have one.  */

static void
check_statement_label (gfc_statement st)
{
  gfc_sl_type type;

  if (gfc_statement_label == NULL)
    {
      if (st == ST_FORMAT)
	gfc_error ("FORMAT statement at %L does not have a statement label",
		   &new_st.loc);
      return;
    }

  switch (st)
    {
    case ST_END_PROGRAM:
    case ST_END_FUNCTION:
    case ST_END_SUBROUTINE:
    case ST_ENDDO:
    case ST_ENDIF:
    case ST_END_SELECT:
    case ST_END_CRITICAL:
    case ST_END_BLOCK:
    case ST_END_ASSOCIATE:
    case_executable:
    case_exec_markers:
      if (st == ST_ENDDO || st == ST_CONTINUE)
	type = ST_LABEL_DO_TARGET;
      else
	type = ST_LABEL_TARGET;
      break;

    case ST_FORMAT:
      type = ST_LABEL_FORMAT;
      break;

      /* Statement labels are not restricted from appearing on a
	 particular line.  However, there are plenty of situations
	 where the resulting label can't be referenced.  */

    default:
      type = ST_LABEL_BAD_TARGET;
      break;
    }

  gfc_define_st_label (gfc_statement_label, type, &label_locus);

  new_st.here = gfc_statement_label;
}


/* Figures out what the enclosing program unit is.  This will be a
   function, subroutine, program, block data or module.  */

gfc_state_data *
gfc_enclosing_unit (gfc_compile_state * result)
{
  gfc_state_data *p;

  for (p = gfc_state_stack; p; p = p->previous)
    if (p->state == COMP_FUNCTION || p->state == COMP_SUBROUTINE
	|| p->state == COMP_MODULE || p->state == COMP_SUBMODULE
	|| p->state == COMP_BLOCK_DATA || p->state == COMP_PROGRAM)
      {

	if (result != NULL)
	  *result = p->state;
	return p;
      }

  if (result != NULL)
    *result = COMP_PROGRAM;
  return NULL;
}


/* Translate a statement enum to a string.  If strip_sentinel is true,
   the !$OMP/!$ACC sentinel is excluded.  */

const char *
gfc_ascii_statement (gfc_statement st, bool strip_sentinel)
{
  const char *p;

  switch (st)
    {
    case ST_ARITHMETIC_IF:
      p = _("arithmetic IF");
      break;
    case ST_ALLOCATE:
      p = "ALLOCATE";
      break;
    case ST_ASSOCIATE:
      p = "ASSOCIATE";
      break;
    case ST_ATTR_DECL:
      p = _("attribute declaration");
      break;
    case ST_BACKSPACE:
      p = "BACKSPACE";
      break;
    case ST_BLOCK:
      p = "BLOCK";
      break;
    case ST_BLOCK_DATA:
      p = "BLOCK DATA";
      break;
    case ST_CALL:
      p = "CALL";
      break;
    case ST_CASE:
      p = "CASE";
      break;
    case ST_CLOSE:
      p = "CLOSE";
      break;
    case ST_COMMON:
      p = "COMMON";
      break;
    case ST_CONTINUE:
      p = "CONTINUE";
      break;
    case ST_CONTAINS:
      p = "CONTAINS";
      break;
    case ST_CRITICAL:
      p = "CRITICAL";
      break;
    case ST_CYCLE:
      p = "CYCLE";
      break;
    case ST_DATA_DECL:
      p = _("data declaration");
      break;
    case ST_DATA:
      p = "DATA";
      break;
    case ST_DEALLOCATE:
      p = "DEALLOCATE";
      break;
    case ST_MAP:
      p = "MAP";
      break;
    case ST_UNION:
      p = "UNION";
      break;
    case ST_STRUCTURE_DECL:
      p = "STRUCTURE";
      break;
    case ST_DERIVED_DECL:
      p = _("derived type declaration");
      break;
    case ST_DO:
      p = "DO";
      break;
    case ST_ELSE:
      p = "ELSE";
      break;
    case ST_ELSEIF:
      p = "ELSE IF";
      break;
    case ST_ELSEWHERE:
      p = "ELSEWHERE";
      break;
    case ST_EVENT_POST:
      p = "EVENT POST";
      break;
    case ST_EVENT_WAIT:
      p = "EVENT WAIT";
      break;
    case ST_FAIL_IMAGE:
      p = "FAIL IMAGE";
      break;
    case ST_CHANGE_TEAM:
      p = "CHANGE TEAM";
      break;
    case ST_END_TEAM:
      p = "END TEAM";
      break;
    case ST_FORM_TEAM:
      p = "FORM TEAM";
      break;
    case ST_SYNC_TEAM:
      p = "SYNC TEAM";
      break;
    case ST_END_ASSOCIATE:
      p = "END ASSOCIATE";
      break;
    case ST_END_BLOCK:
      p = "END BLOCK";
      break;
    case ST_END_BLOCK_DATA:
      p = "END BLOCK DATA";
      break;
    case ST_END_CRITICAL:
      p = "END CRITICAL";
      break;
    case ST_ENDDO:
      p = "END DO";
      break;
    case ST_END_FILE:
      p = "END FILE";
      break;
    case ST_END_FORALL:
      p = "END FORALL";
      break;
    case ST_END_FUNCTION:
      p = "END FUNCTION";
      break;
    case ST_ENDIF:
      p = "END IF";
      break;
    case ST_END_INTERFACE:
      p = "END INTERFACE";
      break;
    case ST_END_MODULE:
      p = "END MODULE";
      break;
    case ST_END_SUBMODULE:
      p = "END SUBMODULE";
      break;
    case ST_END_PROGRAM:
      p = "END PROGRAM";
      break;
    case ST_END_SELECT:
      p = "END SELECT";
      break;
    case ST_END_SUBROUTINE:
      p = "END SUBROUTINE";
      break;
    case ST_END_WHERE:
      p = "END WHERE";
      break;
    case ST_END_STRUCTURE:
      p = "END STRUCTURE";
      break;
    case ST_END_UNION:
      p = "END UNION";
      break;
    case ST_END_MAP:
      p = "END MAP";
      break;
    case ST_END_TYPE:
      p = "END TYPE";
      break;
    case ST_ENTRY:
      p = "ENTRY";
      break;
    case ST_EQUIVALENCE:
      p = "EQUIVALENCE";
      break;
    case ST_ERROR_STOP:
      p = "ERROR STOP";
      break;
    case ST_EXIT:
      p = "EXIT";
      break;
    case ST_FLUSH:
      p = "FLUSH";
      break;
    case ST_FORALL_BLOCK:	/* Fall through */
    case ST_FORALL:
      p = "FORALL";
      break;
    case ST_FORMAT:
      p = "FORMAT";
      break;
    case ST_FUNCTION:
      p = "FUNCTION";
      break;
    case ST_GENERIC:
      p = "GENERIC";
      break;
    case ST_GOTO:
      p = "GOTO";
      break;
    case ST_IF_BLOCK:
      p = _("block IF");
      break;
    case ST_IMPLICIT:
      p = "IMPLICIT";
      break;
    case ST_IMPLICIT_NONE:
      p = "IMPLICIT NONE";
      break;
    case ST_IMPLIED_ENDDO:
      p = _("implied END DO");
      break;
    case ST_IMPORT:
      p = "IMPORT";
      break;
    case ST_INQUIRE:
      p = "INQUIRE";
      break;
    case ST_INTERFACE:
      p = "INTERFACE";
      break;
    case ST_LOCK:
      p = "LOCK";
      break;
    case ST_PARAMETER:
      p = "PARAMETER";
      break;
    case ST_PRIVATE:
      p = "PRIVATE";
      break;
    case ST_PUBLIC:
      p = "PUBLIC";
      break;
    case ST_MODULE:
      p = "MODULE";
      break;
    case ST_SUBMODULE:
      p = "SUBMODULE";
      break;
    case ST_PAUSE:
      p = "PAUSE";
      break;
    case ST_MODULE_PROC:
      p = "MODULE PROCEDURE";
      break;
    case ST_NAMELIST:
      p = "NAMELIST";
      break;
    case ST_NULLIFY:
      p = "NULLIFY";
      break;
    case ST_OPEN:
      p = "OPEN";
      break;
    case ST_PROGRAM:
      p = "PROGRAM";
      break;
    case ST_PROCEDURE:
      p = "PROCEDURE";
      break;
    case ST_READ:
      p = "READ";
      break;
    case ST_RETURN:
      p = "RETURN";
      break;
    case ST_REWIND:
      p = "REWIND";
      break;
    case ST_STOP:
      p = "STOP";
      break;
    case ST_SYNC_ALL:
      p = "SYNC ALL";
      break;
    case ST_SYNC_IMAGES:
      p = "SYNC IMAGES";
      break;
    case ST_SYNC_MEMORY:
      p = "SYNC MEMORY";
      break;
    case ST_SUBROUTINE:
      p = "SUBROUTINE";
      break;
    case ST_TYPE:
      p = "TYPE";
      break;
    case ST_UNLOCK:
      p = "UNLOCK";
      break;
    case ST_USE:
      p = "USE";
      break;
    case ST_WHERE_BLOCK:	/* Fall through */
    case ST_WHERE:
      p = "WHERE";
      break;
    case ST_WAIT:
      p = "WAIT";
      break;
    case ST_WRITE:
      p = "WRITE";
      break;
    case ST_ASSIGNMENT:
      p = _("assignment");
      break;
    case ST_POINTER_ASSIGNMENT:
      p = _("pointer assignment");
      break;
    case ST_SELECT_CASE:
      p = "SELECT CASE";
      break;
    case ST_SELECT_TYPE:
      p = "SELECT TYPE";
      break;
    case ST_SELECT_RANK:
      p = "SELECT RANK";
      break;
    case ST_TYPE_IS:
      p = "TYPE IS";
      break;
    case ST_CLASS_IS:
      p = "CLASS IS";
      break;
    case ST_RANK:
      p = "RANK";
      break;
    case ST_SEQUENCE:
      p = "SEQUENCE";
      break;
    case ST_SIMPLE_IF:
      p = _("simple IF");
      break;
    case ST_STATEMENT_FUNCTION:
      p = "STATEMENT FUNCTION";
      break;
    case ST_LABEL_ASSIGNMENT:
      p = "LABEL ASSIGNMENT";
      break;
    case ST_ENUM:
      p = "ENUM DEFINITION";
      break;
    case ST_ENUMERATOR:
      p = "ENUMERATOR DEFINITION";
      break;
    case ST_END_ENUM:
      p = "END ENUM";
      break;
    case ST_OACC_PARALLEL_LOOP:
      p = "!$ACC PARALLEL LOOP";
      break;
    case ST_OACC_END_PARALLEL_LOOP:
      p = "!$ACC END PARALLEL LOOP";
      break;
    case ST_OACC_PARALLEL:
      p = "!$ACC PARALLEL";
      break;
    case ST_OACC_END_PARALLEL:
      p = "!$ACC END PARALLEL";
      break;
    case ST_OACC_KERNELS:
      p = "!$ACC KERNELS";
      break;
    case ST_OACC_END_KERNELS:
      p = "!$ACC END KERNELS";
      break;
    case ST_OACC_KERNELS_LOOP:
      p = "!$ACC KERNELS LOOP";
      break;
    case ST_OACC_END_KERNELS_LOOP:
      p = "!$ACC END KERNELS LOOP";
      break;
    case ST_OACC_SERIAL_LOOP:
      p = "!$ACC SERIAL LOOP";
      break;
    case ST_OACC_END_SERIAL_LOOP:
      p = "!$ACC END SERIAL LOOP";
      break;
    case ST_OACC_SERIAL:
      p = "!$ACC SERIAL";
      break;
    case ST_OACC_END_SERIAL:
      p = "!$ACC END SERIAL";
      break;
    case ST_OACC_DATA:
      p = "!$ACC DATA";
      break;
    case ST_OACC_END_DATA:
      p = "!$ACC END DATA";
      break;
    case ST_OACC_HOST_DATA:
      p = "!$ACC HOST_DATA";
      break;
    case ST_OACC_END_HOST_DATA:
      p = "!$ACC END HOST_DATA";
      break;
    case ST_OACC_LOOP:
      p = "!$ACC LOOP";
      break;
    case ST_OACC_END_LOOP:
      p = "!$ACC END LOOP";
      break;
    case ST_OACC_DECLARE:
      p = "!$ACC DECLARE";
      break;
    case ST_OACC_UPDATE:
      p = "!$ACC UPDATE";
      break;
    case ST_OACC_WAIT:
      p = "!$ACC WAIT";
      break;
    case ST_OACC_CACHE:
      p = "!$ACC CACHE";
      break;
    case ST_OACC_ENTER_DATA:
      p = "!$ACC ENTER DATA";
      break;
    case ST_OACC_EXIT_DATA:
      p = "!$ACC EXIT DATA";
      break;
    case ST_OACC_ROUTINE:
      p = "!$ACC ROUTINE";
      break;
    case ST_OACC_ATOMIC:
      p = "!$ACC ATOMIC";
      break;
    case ST_OACC_END_ATOMIC:
      p = "!$ACC END ATOMIC";
      break;
    case ST_OMP_ALLOCATE:
    case ST_OMP_ALLOCATE_EXEC:
      p = "!$OMP ALLOCATE";
      break;
    case ST_OMP_ALLOCATORS:
      p = "!$OMP ALLOCATORS";
      break;
    case ST_OMP_ASSUME:
      p = "!$OMP ASSUME";
      break;
    case ST_OMP_ASSUMES:
      p = "!$OMP ASSUMES";
      break;
    case ST_OMP_ATOMIC:
      p = "!$OMP ATOMIC";
      break;
    case ST_OMP_BARRIER:
      p = "!$OMP BARRIER";
      break;
    case ST_OMP_CANCEL:
      p = "!$OMP CANCEL";
      break;
    case ST_OMP_CANCELLATION_POINT:
      p = "!$OMP CANCELLATION POINT";
      break;
    case ST_OMP_CRITICAL:
      p = "!$OMP CRITICAL";
      break;
    case ST_OMP_DECLARE_REDUCTION:
      p = "!$OMP DECLARE REDUCTION";
      break;
    case ST_OMP_DECLARE_SIMD:
      p = "!$OMP DECLARE SIMD";
      break;
    case ST_OMP_DECLARE_TARGET:
      p = "!$OMP DECLARE TARGET";
      break;
    case ST_OMP_DECLARE_VARIANT:
      p = "!$OMP DECLARE VARIANT";
      break;
    case ST_OMP_DEPOBJ:
      p = "!$OMP DEPOBJ";
      break;
    case ST_OMP_DISTRIBUTE:
      p = "!$OMP DISTRIBUTE";
      break;
    case ST_OMP_DISTRIBUTE_PARALLEL_DO:
      p = "!$OMP DISTRIBUTE PARALLEL DO";
      break;
    case ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      p = "!$OMP DISTRIBUTE PARALLEL DO SIMD";
      break;
    case ST_OMP_DISTRIBUTE_SIMD:
      p = "!$OMP DISTRIBUTE SIMD";
      break;
    case ST_OMP_DO:
      p = "!$OMP DO";
      break;
    case ST_OMP_DO_SIMD:
      p = "!$OMP DO SIMD";
      break;
    case ST_OMP_END_ALLOCATORS:
      p = "!$OMP END ALLOCATORS";
      break;
    case ST_OMP_END_ASSUME:
      p = "!$OMP END ASSUME";
      break;
    case ST_OMP_END_ATOMIC:
      p = "!$OMP END ATOMIC";
      break;
    case ST_OMP_END_CRITICAL:
      p = "!$OMP END CRITICAL";
      break;
    case ST_OMP_END_DISTRIBUTE:
      p = "!$OMP END DISTRIBUTE";
      break;
    case ST_OMP_END_DISTRIBUTE_PARALLEL_DO:
      p = "!$OMP END DISTRIBUTE PARALLEL DO";
      break;
    case ST_OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD:
      p = "!$OMP END DISTRIBUTE PARALLEL DO SIMD";
      break;
    case ST_OMP_END_DISTRIBUTE_SIMD:
      p = "!$OMP END DISTRIBUTE SIMD";
      break;
    case ST_OMP_END_DO:
      p = "!$OMP END DO";
      break;
    case ST_OMP_END_DO_SIMD:
      p = "!$OMP END DO SIMD";
      break;
    case ST_OMP_END_SCOPE:
      p = "!$OMP END SCOPE";
      break;
    case ST_OMP_END_SIMD:
      p = "!$OMP END SIMD";
      break;
    case ST_OMP_END_LOOP:
      p = "!$OMP END LOOP";
      break;
    case ST_OMP_END_MASKED:
      p = "!$OMP END MASKED";
      break;
    case ST_OMP_END_MASKED_TASKLOOP:
      p = "!$OMP END MASKED TASKLOOP";
      break;
    case ST_OMP_END_MASKED_TASKLOOP_SIMD:
      p = "!$OMP END MASKED TASKLOOP SIMD";
      break;
    case ST_OMP_END_MASTER:
      p = "!$OMP END MASTER";
      break;
    case ST_OMP_END_MASTER_TASKLOOP:
      p = "!$OMP END MASTER TASKLOOP";
      break;
    case ST_OMP_END_MASTER_TASKLOOP_SIMD:
      p = "!$OMP END MASTER TASKLOOP SIMD";
      break;
    case ST_OMP_END_ORDERED:
      p = "!$OMP END ORDERED";
      break;
    case ST_OMP_END_PARALLEL:
      p = "!$OMP END PARALLEL";
      break;
    case ST_OMP_END_PARALLEL_DO:
      p = "!$OMP END PARALLEL DO";
      break;
    case ST_OMP_END_PARALLEL_DO_SIMD:
      p = "!$OMP END PARALLEL DO SIMD";
      break;
    case ST_OMP_END_PARALLEL_LOOP:
      p = "!$OMP END PARALLEL LOOP";
      break;
    case ST_OMP_END_PARALLEL_MASKED:
      p = "!$OMP END PARALLEL MASKED";
      break;
    case ST_OMP_END_PARALLEL_MASKED_TASKLOOP:
      p = "!$OMP END PARALLEL MASKED TASKLOOP";
      break;
    case ST_OMP_END_PARALLEL_MASKED_TASKLOOP_SIMD:
      p = "!$OMP END PARALLEL MASKED TASKLOOP SIMD";
      break;
    case ST_OMP_END_PARALLEL_MASTER:
      p = "!$OMP END PARALLEL MASTER";
      break;
    case ST_OMP_END_PARALLEL_MASTER_TASKLOOP:
      p = "!$OMP END PARALLEL MASTER TASKLOOP";
      break;
    case ST_OMP_END_PARALLEL_MASTER_TASKLOOP_SIMD:
      p = "!$OMP END PARALLEL MASTER TASKLOOP SIMD";
      break;
    case ST_OMP_END_PARALLEL_SECTIONS:
      p = "!$OMP END PARALLEL SECTIONS";
      break;
    case ST_OMP_END_PARALLEL_WORKSHARE:
      p = "!$OMP END PARALLEL WORKSHARE";
      break;
    case ST_OMP_END_SECTIONS:
      p = "!$OMP END SECTIONS";
      break;
    case ST_OMP_END_SINGLE:
      p = "!$OMP END SINGLE";
      break;
    case ST_OMP_END_TASK:
      p = "!$OMP END TASK";
      break;
    case ST_OMP_END_TARGET:
      p = "!$OMP END TARGET";
      break;
    case ST_OMP_END_TARGET_DATA:
      p = "!$OMP END TARGET DATA";
      break;
    case ST_OMP_END_TARGET_PARALLEL:
      p = "!$OMP END TARGET PARALLEL";
      break;
    case ST_OMP_END_TARGET_PARALLEL_DO:
      p = "!$OMP END TARGET PARALLEL DO";
      break;
    case ST_OMP_END_TARGET_PARALLEL_DO_SIMD:
      p = "!$OMP END TARGET PARALLEL DO SIMD";
      break;
    case ST_OMP_END_TARGET_PARALLEL_LOOP:
      p = "!$OMP END TARGET PARALLEL LOOP";
      break;
    case ST_OMP_END_TARGET_SIMD:
      p = "!$OMP END TARGET SIMD";
      break;
    case ST_OMP_END_TARGET_TEAMS:
      p = "!$OMP END TARGET TEAMS";
      break;
    case ST_OMP_END_TARGET_TEAMS_DISTRIBUTE:
      p = "!$OMP END TARGET TEAMS DISTRIBUTE";
      break;
    case ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      p = "!$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO";
      break;
    case ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      p = "!$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD";
      break;
    case ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD:
      p = "!$OMP END TARGET TEAMS DISTRIBUTE SIMD";
      break;
    case ST_OMP_END_TARGET_TEAMS_LOOP:
      p = "!$OMP END TARGET TEAMS LOOP";
      break;
    case ST_OMP_END_TASKGROUP:
      p = "!$OMP END TASKGROUP";
      break;
    case ST_OMP_END_TASKLOOP:
      p = "!$OMP END TASKLOOP";
      break;
    case ST_OMP_END_TASKLOOP_SIMD:
      p = "!$OMP END TASKLOOP SIMD";
      break;
    case ST_OMP_END_TEAMS:
      p = "!$OMP END TEAMS";
      break;
    case ST_OMP_END_TEAMS_DISTRIBUTE:
      p = "!$OMP END TEAMS DISTRIBUTE";
      break;
    case ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO:
      p = "!$OMP END TEAMS DISTRIBUTE PARALLEL DO";
      break;
    case ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      p = "!$OMP END TEAMS DISTRIBUTE PARALLEL DO SIMD";
      break;
    case ST_OMP_END_TEAMS_DISTRIBUTE_SIMD:
      p = "!$OMP END TEAMS DISTRIBUTE SIMD";
      break;
    case ST_OMP_END_TEAMS_LOOP:
      p = "!$OMP END TEAMS LOOP";
      break;
    case ST_OMP_END_WORKSHARE:
      p = "!$OMP END WORKSHARE";
      break;
    case ST_OMP_ERROR:
      p = "!$OMP ERROR";
      break;
    case ST_OMP_FLUSH:
      p = "!$OMP FLUSH";
      break;
    case ST_OMP_LOOP:
      p = "!$OMP LOOP";
      break;
    case ST_OMP_MASKED:
      p = "!$OMP MASKED";
      break;
    case ST_OMP_MASKED_TASKLOOP:
      p = "!$OMP MASKED TASKLOOP";
      break;
    case ST_OMP_MASKED_TASKLOOP_SIMD:
      p = "!$OMP MASKED TASKLOOP SIMD";
      break;
    case ST_OMP_MASTER:
      p = "!$OMP MASTER";
      break;
    case ST_OMP_MASTER_TASKLOOP:
      p = "!$OMP MASTER TASKLOOP";
      break;
    case ST_OMP_MASTER_TASKLOOP_SIMD:
      p = "!$OMP MASTER TASKLOOP SIMD";
      break;
    case ST_OMP_ORDERED:
    case ST_OMP_ORDERED_DEPEND:
      p = "!$OMP ORDERED";
      break;
    case ST_OMP_NOTHING:
      /* Note: gfc_match_omp_nothing returns ST_NONE. */
      p = "!$OMP NOTHING";
      break;
    case ST_OMP_PARALLEL:
      p = "!$OMP PARALLEL";
      break;
    case ST_OMP_PARALLEL_DO:
      p = "!$OMP PARALLEL DO";
      break;
    case ST_OMP_PARALLEL_LOOP:
      p = "!$OMP PARALLEL LOOP";
      break;
    case ST_OMP_PARALLEL_DO_SIMD:
      p = "!$OMP PARALLEL DO SIMD";
      break;
    case ST_OMP_PARALLEL_MASKED:
      p = "!$OMP PARALLEL MASKED";
      break;
    case ST_OMP_PARALLEL_MASKED_TASKLOOP:
      p = "!$OMP PARALLEL MASKED TASKLOOP";
      break;
    case ST_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
      p = "!$OMP PARALLEL MASKED TASKLOOP SIMD";
      break;
    case ST_OMP_PARALLEL_MASTER:
      p = "!$OMP PARALLEL MASTER";
      break;
    case ST_OMP_PARALLEL_MASTER_TASKLOOP:
      p = "!$OMP PARALLEL MASTER TASKLOOP";
      break;
    case ST_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      p = "!$OMP PARALLEL MASTER TASKLOOP SIMD";
      break;
    case ST_OMP_PARALLEL_SECTIONS:
      p = "!$OMP PARALLEL SECTIONS";
      break;
    case ST_OMP_PARALLEL_WORKSHARE:
      p = "!$OMP PARALLEL WORKSHARE";
      break;
    case ST_OMP_REQUIRES:
      p = "!$OMP REQUIRES";
      break;
    case ST_OMP_SCAN:
      p = "!$OMP SCAN";
      break;
    case ST_OMP_SCOPE:
      p = "!$OMP SCOPE";
      break;
    case ST_OMP_SECTIONS:
      p = "!$OMP SECTIONS";
      break;
    case ST_OMP_SECTION:
      p = "!$OMP SECTION";
      break;
    case ST_OMP_SIMD:
      p = "!$OMP SIMD";
      break;
    case ST_OMP_SINGLE:
      p = "!$OMP SINGLE";
      break;
    case ST_OMP_TARGET:
      p = "!$OMP TARGET";
      break;
    case ST_OMP_TARGET_DATA:
      p = "!$OMP TARGET DATA";
      break;
    case ST_OMP_TARGET_ENTER_DATA:
      p = "!$OMP TARGET ENTER DATA";
      break;
    case ST_OMP_TARGET_EXIT_DATA:
      p = "!$OMP TARGET EXIT DATA";
      break;
    case ST_OMP_TARGET_PARALLEL:
      p = "!$OMP TARGET PARALLEL";
      break;
    case ST_OMP_TARGET_PARALLEL_DO:
      p = "!$OMP TARGET PARALLEL DO";
      break;
    case ST_OMP_TARGET_PARALLEL_DO_SIMD:
      p = "!$OMP TARGET PARALLEL DO SIMD";
      break;
    case ST_OMP_TARGET_PARALLEL_LOOP:
      p = "!$OMP TARGET PARALLEL LOOP";
      break;
    case ST_OMP_TARGET_SIMD:
      p = "!$OMP TARGET SIMD";
      break;
    case ST_OMP_TARGET_TEAMS:
      p = "!$OMP TARGET TEAMS";
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE:
      p = "!$OMP TARGET TEAMS DISTRIBUTE";
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      p = "!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO";
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      p = "!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD";
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
      p = "!$OMP TARGET TEAMS DISTRIBUTE SIMD";
      break;
    case ST_OMP_TARGET_TEAMS_LOOP:
      p = "!$OMP TARGET TEAMS LOOP";
      break;
    case ST_OMP_TARGET_UPDATE:
      p = "!$OMP TARGET UPDATE";
      break;
    case ST_OMP_TASK:
      p = "!$OMP TASK";
      break;
    case ST_OMP_TASKGROUP:
      p = "!$OMP TASKGROUP";
      break;
    case ST_OMP_TASKLOOP:
      p = "!$OMP TASKLOOP";
      break;
    case ST_OMP_TASKLOOP_SIMD:
      p = "!$OMP TASKLOOP SIMD";
      break;
    case ST_OMP_TASKWAIT:
      p = "!$OMP TASKWAIT";
      break;
    case ST_OMP_TASKYIELD:
      p = "!$OMP TASKYIELD";
      break;
    case ST_OMP_TEAMS:
      p = "!$OMP TEAMS";
      break;
    case ST_OMP_TEAMS_DISTRIBUTE:
      p = "!$OMP TEAMS DISTRIBUTE";
      break;
    case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      p = "!$OMP TEAMS DISTRIBUTE PARALLEL DO";
      break;
    case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      p = "!$OMP TEAMS DISTRIBUTE PARALLEL DO SIMD";
      break;
    case ST_OMP_TEAMS_DISTRIBUTE_SIMD:
      p = "!$OMP TEAMS DISTRIBUTE SIMD";
      break;
    case ST_OMP_TEAMS_LOOP:
      p = "!$OMP TEAMS LOOP";
      break;
    case ST_OMP_THREADPRIVATE:
      p = "!$OMP THREADPRIVATE";
      break;
    case ST_OMP_WORKSHARE:
      p = "!$OMP WORKSHARE";
      break;
    default:
      gfc_internal_error ("gfc_ascii_statement(): Bad statement code");
    }

  if (strip_sentinel && p[0] == '!')
    return p + strlen ("!$OMP ");
  return p;
}


/* Create a symbol for the main program and assign it to ns->proc_name.  */

static void
main_program_symbol (gfc_namespace *ns, const char *name)
{
  gfc_symbol *main_program;
  symbol_attribute attr;

  gfc_get_symbol (name, ns, &main_program);
  gfc_clear_attr (&attr);
  attr.flavor = FL_PROGRAM;
  attr.proc = PROC_UNKNOWN;
  attr.subroutine = 1;
  attr.access = ACCESS_PUBLIC;
  attr.is_main_program = 1;
  main_program->attr = attr;
  main_program->declared_at = gfc_current_locus;
  ns->proc_name = main_program;
  gfc_commit_symbols ();
}


/* Do whatever is necessary to accept the last statement.  */

static void
accept_statement (gfc_statement st)
{
  switch (st)
    {
    case ST_IMPLICIT_NONE:
    case ST_IMPLICIT:
      break;

    case ST_FUNCTION:
    case ST_SUBROUTINE:
    case ST_MODULE:
    case ST_SUBMODULE:
      gfc_current_ns->proc_name = gfc_new_block;
      break;

      /* If the statement is the end of a block, lay down a special code
	 that allows a branch to the end of the block from within the
	 construct.  IF and SELECT are treated differently from DO
	 (where EXEC_NOP is added inside the loop) for two
	 reasons:
         1. END DO has a meaning in the sense that after a GOTO to
	    it, the loop counter must be increased.
         2. IF blocks and SELECT blocks can consist of multiple
	    parallel blocks (IF ... ELSE IF ... ELSE ... END IF).
	    Putting the label before the END IF would make the jump
	    from, say, the ELSE IF block to the END IF illegal.  */

    case ST_ENDIF:
    case ST_END_SELECT:
    case ST_END_CRITICAL:
      if (gfc_statement_label != NULL)
	{
	  new_st.op = EXEC_END_NESTED_BLOCK;
	  add_statement ();
	}
      break;

      /* In the case of BLOCK and ASSOCIATE blocks, there cannot be more than
	 one parallel block.  Thus, we add the special code to the nested block
	 itself, instead of the parent one.  */
    case ST_END_BLOCK:
    case ST_END_ASSOCIATE:
      if (gfc_statement_label != NULL)
	{
	  new_st.op = EXEC_END_BLOCK;
	  add_statement ();
	}
      break;

      /* The end-of-program unit statements do not get the special
	 marker and require a statement of some sort if they are a
	 branch target.  */

    case ST_END_PROGRAM:
    case ST_END_FUNCTION:
    case ST_END_SUBROUTINE:
      if (gfc_statement_label != NULL)
	{
	  new_st.op = EXEC_RETURN;
	  add_statement ();
	}
      else
	{
	  new_st.op = EXEC_END_PROCEDURE;
	  add_statement ();
	}

      break;

    case ST_ENTRY:
    case_executable:
    case_exec_markers:
      add_statement ();
      break;

    default:
      break;
    }

  gfc_commit_symbols ();
  gfc_warning_check ();
  gfc_clear_new_st ();
}


/* Undo anything tentative that has been built for the current statement,
   except if a gfc_charlen structure has been added to current namespace's
   list of gfc_charlen structure.  */

static void
reject_statement (void)
{
  gfc_free_equiv_until (gfc_current_ns->equiv, gfc_current_ns->old_equiv);
  gfc_current_ns->equiv = gfc_current_ns->old_equiv;
  gfc_drop_interface_elements_before (current_interface_ptr,
				      previous_interface_head);

  gfc_reject_data (gfc_current_ns);

  /* Don't queue use-association of a module if we reject the use statement.  */
  gfc_restore_old_module_list ();

  gfc_new_block = NULL;
  gfc_undo_symbols ();
  gfc_clear_warning ();
  undo_new_statement ();
}


/* Generic complaint about an out of order statement.  We also do
   whatever is necessary to clean up.  */

static void
unexpected_statement (gfc_statement st)
{
  gfc_error ("Unexpected %s statement at %C", gfc_ascii_statement (st));

  reject_statement ();
}


/* Given the next statement seen by the matcher, make sure that it is
   in proper order with the last.  This subroutine is initialized by
   calling it with an argument of ST_NONE.  If there is a problem, we
   issue an error and return false.  Otherwise we return true.

   Individual parsers need to verify that the statements seen are
   valid before calling here, i.e., ENTRY statements are not allowed in
   INTERFACE blocks.  The following diagram is taken from the standard:

	    +---------------------------------------+
	    | program  subroutine  function  module |
	    +---------------------------------------+
	    |		 use		   |
	    +---------------------------------------+
	    |		 import		|
	    +---------------------------------------+
	    |	|	implicit none	 |
	    |	+-----------+------------------+
	    |	| parameter |  implicit	|
	    |	+-----------+------------------+
	    | format |	   |  derived type    |
	    | entry  | parameter |  interface       |
	    |	|   data    |  specification   |
	    |	|	   |  statement func  |
	    |	+-----------+------------------+
	    |	|   data    |    executable    |
	    +--------+-----------+------------------+
	    |		contains	       |
	    +---------------------------------------+
	    |      internal module/subprogram       |
	    +---------------------------------------+
	    |		   end		 |
	    +---------------------------------------+

*/

enum state_order
{
  ORDER_START,
  ORDER_USE,
  ORDER_IMPORT,
  ORDER_IMPLICIT_NONE,
  ORDER_IMPLICIT,
  ORDER_SPEC,
  ORDER_EXEC
};

typedef struct
{
  enum state_order state;
  gfc_statement last_statement;
  locus where;
}
st_state;

static bool
verify_st_order (st_state *p, gfc_statement st, bool silent)
{

  switch (st)
    {
    case ST_NONE:
      p->state = ORDER_START;
      in_exec_part = false;
      break;

    case ST_USE:
      if (p->state > ORDER_USE)
	goto order;
      p->state = ORDER_USE;
      break;

    case ST_IMPORT:
      if (p->state > ORDER_IMPORT)
	goto order;
      p->state = ORDER_IMPORT;
      break;

    case ST_IMPLICIT_NONE:
      if (p->state > ORDER_IMPLICIT)
	goto order;

      /* The '>' sign cannot be a '>=', because a FORMAT or ENTRY
	 statement disqualifies a USE but not an IMPLICIT NONE.
	 Duplicate IMPLICIT NONEs are caught when the implicit types
	 are set.  */

      p->state = ORDER_IMPLICIT_NONE;
      break;

    case ST_IMPLICIT:
      if (p->state > ORDER_IMPLICIT)
	goto order;
      p->state = ORDER_IMPLICIT;
      break;

    case ST_FORMAT:
    case ST_ENTRY:
      if (p->state < ORDER_IMPLICIT_NONE)
	p->state = ORDER_IMPLICIT_NONE;
      break;

    case ST_PARAMETER:
      if (p->state >= ORDER_EXEC)
	goto order;
      if (p->state < ORDER_IMPLICIT)
	p->state = ORDER_IMPLICIT;
      break;

    case ST_DATA:
      if (p->state < ORDER_SPEC)
	p->state = ORDER_SPEC;
      break;

    case ST_PUBLIC:
    case ST_PRIVATE:
    case ST_STRUCTURE_DECL:
    case ST_DERIVED_DECL:
    case_decl:
      if (p->state >= ORDER_EXEC)
	goto order;
      if (p->state < ORDER_SPEC)
	p->state = ORDER_SPEC;
      break;

    case_omp_decl:
      /* The OpenMP/OpenACC directives have to be somewhere in the specification
	 part, but there are no further requirements on their ordering.
	 Thus don't adjust p->state, just ignore them.  */
      if (p->state >= ORDER_EXEC)
	goto order;
      break;

    case_executable:
    case_exec_markers:
      if (p->state < ORDER_EXEC)
	p->state = ORDER_EXEC;
      in_exec_part = true;
      break;

    default:
      return false;
    }

  /* All is well, record the statement in case we need it next time.  */
  p->where = gfc_current_locus;
  p->last_statement = st;
  return true;

order:
  if (!silent)
    gfc_error ("%s statement at %C cannot follow %s statement at %L",
	       gfc_ascii_statement (st),
	       gfc_ascii_statement (p->last_statement), &p->where);

  return false;
}


/* Handle an unexpected end of file.  This is a show-stopper...  */

static void unexpected_eof (void) ATTRIBUTE_NORETURN;

static void
unexpected_eof (void)
{
  gfc_state_data *p;

  gfc_error ("Unexpected end of file in %qs", gfc_source_file);

  /* Memory cleanup.  Move to "second to last".  */
  for (p = gfc_state_stack; p && p->previous && p->previous->previous;
       p = p->previous);

  gfc_current_ns->code = (p && p->previous) ? p->head : NULL;
  gfc_done_2 ();

  longjmp (eof_buf, 1);

  /* Avoids build error on systems where longjmp is not declared noreturn.  */
  gcc_unreachable ();
}


/* Parse the CONTAINS section of a derived type definition.  */

gfc_access gfc_typebound_default_access;

static bool
parse_derived_contains (void)
{
  gfc_state_data s;
  bool seen_private = false;
  bool seen_comps = false;
  bool error_flag = false;
  bool to_finish;

  gcc_assert (gfc_current_state () == COMP_DERIVED);
  gcc_assert (gfc_current_block ());

  /* Derived-types with SEQUENCE and/or BIND(C) must not have a CONTAINS
     section.  */
  if (gfc_current_block ()->attr.sequence)
    gfc_error ("Derived-type %qs with SEQUENCE must not have a CONTAINS"
	       " section at %C", gfc_current_block ()->name);
  if (gfc_current_block ()->attr.is_bind_c)
    gfc_error ("Derived-type %qs with BIND(C) must not have a CONTAINS"
	       " section at %C", gfc_current_block ()->name);

  accept_statement (ST_CONTAINS);
  push_state (&s, COMP_DERIVED_CONTAINS, NULL);

  gfc_typebound_default_access = ACCESS_PUBLIC;

  to_finish = false;
  while (!to_finish)
    {
      gfc_statement st;
      st = next_statement ();
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();
	  break;

	case ST_DATA_DECL:
	  gfc_error ("Components in TYPE at %C must precede CONTAINS");
	  goto error;

	case ST_PROCEDURE:
	  if (!gfc_notify_std (GFC_STD_F2003, "Type-bound procedure at %C"))
	    goto error;

	  accept_statement (ST_PROCEDURE);
	  seen_comps = true;
	  break;

	case ST_GENERIC:
	  if (!gfc_notify_std (GFC_STD_F2003, "GENERIC binding at %C"))
	    goto error;

	  accept_statement (ST_GENERIC);
	  seen_comps = true;
	  break;

	case ST_FINAL:
	  if (!gfc_notify_std (GFC_STD_F2003, "FINAL procedure declaration"
			       " at %C"))
	    goto error;

	  accept_statement (ST_FINAL);
	  seen_comps = true;
	  break;

	case ST_END_TYPE:
	  to_finish = true;

	  if (!seen_comps
	      && (!gfc_notify_std(GFC_STD_F2008, "Derived type definition "
				  "at %C with empty CONTAINS section")))
	    goto error;

	  /* ST_END_TYPE is accepted by parse_derived after return.  */
	  break;

	case ST_PRIVATE:
	  if (!gfc_find_state (COMP_MODULE))
	    {
	      gfc_error ("PRIVATE statement in TYPE at %C must be inside "
			 "a MODULE");
	      goto error;
	    }

	  if (seen_comps)
	    {
	      gfc_error ("PRIVATE statement at %C must precede procedure"
			 " bindings");
	      goto error;
	    }

	  if (seen_private)
	    {
	      gfc_error ("Duplicate PRIVATE statement at %C");
	      goto error;
	    }

	  accept_statement (ST_PRIVATE);
	  gfc_typebound_default_access = ACCESS_PRIVATE;
	  seen_private = true;
	  break;

	case ST_SEQUENCE:
	  gfc_error ("SEQUENCE statement at %C must precede CONTAINS");
	  goto error;

	case ST_CONTAINS:
	  gfc_error ("Already inside a CONTAINS block at %C");
	  goto error;

	default:
	  unexpected_statement (st);
	  break;
	}

      continue;

error:
      error_flag = true;
      reject_statement ();
    }

  pop_state ();
  gcc_assert (gfc_current_state () == COMP_DERIVED);

  return error_flag;
}


/* Set attributes for the parent symbol based on the attributes of a component
   and raise errors if conflicting attributes are found for the component.  */

static void
check_component (gfc_symbol *sym, gfc_component *c, gfc_component **lockp,
    gfc_component **eventp)
{
  bool coarray, lock_type, event_type, allocatable, pointer;
  coarray = lock_type = event_type = allocatable = pointer = false;
  gfc_component *lock_comp = NULL, *event_comp = NULL;

  if (lockp) lock_comp = *lockp;
  if (eventp) event_comp = *eventp;

  /* Look for allocatable components.  */
  if (c->attr.allocatable
      || (c->ts.type == BT_CLASS && c->attr.class_ok
          && CLASS_DATA (c)->attr.allocatable)
      || (c->ts.type == BT_DERIVED && !c->attr.pointer
          && c->ts.u.derived->attr.alloc_comp))
    {
      allocatable = true;
      sym->attr.alloc_comp = 1;
    }

  /* Look for pointer components.  */
  if (c->attr.pointer
      || (c->ts.type == BT_CLASS && c->attr.class_ok
          && CLASS_DATA (c)->attr.class_pointer)
      || (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.pointer_comp))
    {
      pointer = true;
      sym->attr.pointer_comp = 1;
    }

  /* Look for procedure pointer components.  */
  if (c->attr.proc_pointer
      || (c->ts.type == BT_DERIVED
          && c->ts.u.derived->attr.proc_pointer_comp))
    sym->attr.proc_pointer_comp = 1;

  /* Looking for coarray components.  */
  if (c->attr.codimension
      || (c->ts.type == BT_CLASS && c->attr.class_ok
          && CLASS_DATA (c)->attr.codimension))
    {
      coarray = true;
      sym->attr.coarray_comp = 1;
    }

  if (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.coarray_comp
      && !c->attr.pointer)
    {
      coarray = true;
      sym->attr.coarray_comp = 1;
    }

  /* Looking for lock_type components.  */
  if ((c->ts.type == BT_DERIVED
          && c->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
          && c->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE)
      || (c->ts.type == BT_CLASS && c->attr.class_ok
          && CLASS_DATA (c)->ts.u.derived->from_intmod
             == INTMOD_ISO_FORTRAN_ENV
          && CLASS_DATA (c)->ts.u.derived->intmod_sym_id
             == ISOFORTRAN_LOCK_TYPE)
      || (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.lock_comp
          && !allocatable && !pointer))
    {
      lock_type = 1;
      lock_comp = c;
      sym->attr.lock_comp = 1;
    }

    /* Looking for event_type components.  */
    if ((c->ts.type == BT_DERIVED
            && c->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
            && c->ts.u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE)
        || (c->ts.type == BT_CLASS && c->attr.class_ok
            && CLASS_DATA (c)->ts.u.derived->from_intmod
               == INTMOD_ISO_FORTRAN_ENV
            && CLASS_DATA (c)->ts.u.derived->intmod_sym_id
               == ISOFORTRAN_EVENT_TYPE)
        || (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.event_comp
            && !allocatable && !pointer))
      {
        event_type = 1;
        event_comp = c;
        sym->attr.event_comp = 1;
      }

  /* Check for F2008, C1302 - and recall that pointers may not be coarrays
     (5.3.14) and that subobjects of coarray are coarray themselves (2.4.7),
     unless there are nondirect [allocatable or pointer] components
     involved (cf. 1.3.33.1 and 1.3.33.3).  */

  if (pointer && !coarray && lock_type)
    gfc_error ("Component %s at %L of type LOCK_TYPE must have a "
               "codimension or be a subcomponent of a coarray, "
               "which is not possible as the component has the "
               "pointer attribute", c->name, &c->loc);
  else if (pointer && !coarray && c->ts.type == BT_DERIVED
           && c->ts.u.derived->attr.lock_comp)
    gfc_error ("Pointer component %s at %L has a noncoarray subcomponent "
               "of type LOCK_TYPE, which must have a codimension or be a "
               "subcomponent of a coarray", c->name, &c->loc);

  if (lock_type && allocatable && !coarray)
    gfc_error ("Allocatable component %s at %L of type LOCK_TYPE must have "
               "a codimension", c->name, &c->loc);
  else if (lock_type && allocatable && c->ts.type == BT_DERIVED
           && c->ts.u.derived->attr.lock_comp)
    gfc_error ("Allocatable component %s at %L must have a codimension as "
               "it has a noncoarray subcomponent of type LOCK_TYPE",
               c->name, &c->loc);

  if (sym->attr.coarray_comp && !coarray && lock_type)
    gfc_error ("Noncoarray component %s at %L of type LOCK_TYPE or with "
               "subcomponent of type LOCK_TYPE must have a codimension or "
               "be a subcomponent of a coarray. (Variables of type %s may "
               "not have a codimension as already a coarray "
               "subcomponent exists)", c->name, &c->loc, sym->name);

  if (sym->attr.lock_comp && coarray && !lock_type)
    gfc_error ("Noncoarray component %s at %L of type LOCK_TYPE or with "
               "subcomponent of type LOCK_TYPE must have a codimension or "
               "be a subcomponent of a coarray. (Variables of type %s may "
               "not have a codimension as %s at %L has a codimension or a "
               "coarray subcomponent)", lock_comp->name, &lock_comp->loc,
               sym->name, c->name, &c->loc);

  /* Similarly for EVENT TYPE.  */

  if (pointer && !coarray && event_type)
    gfc_error ("Component %s at %L of type EVENT_TYPE must have a "
               "codimension or be a subcomponent of a coarray, "
               "which is not possible as the component has the "
               "pointer attribute", c->name, &c->loc);
  else if (pointer && !coarray && c->ts.type == BT_DERIVED
           && c->ts.u.derived->attr.event_comp)
    gfc_error ("Pointer component %s at %L has a noncoarray subcomponent "
               "of type EVENT_TYPE, which must have a codimension or be a "
               "subcomponent of a coarray", c->name, &c->loc);

  if (event_type && allocatable && !coarray)
    gfc_error ("Allocatable component %s at %L of type EVENT_TYPE must have "
               "a codimension", c->name, &c->loc);
  else if (event_type && allocatable && c->ts.type == BT_DERIVED
           && c->ts.u.derived->attr.event_comp)
    gfc_error ("Allocatable component %s at %L must have a codimension as "
               "it has a noncoarray subcomponent of type EVENT_TYPE",
               c->name, &c->loc);

  if (sym->attr.coarray_comp && !coarray && event_type)
    gfc_error ("Noncoarray component %s at %L of type EVENT_TYPE or with "
               "subcomponent of type EVENT_TYPE must have a codimension or "
               "be a subcomponent of a coarray. (Variables of type %s may "
               "not have a codimension as already a coarray "
               "subcomponent exists)", c->name, &c->loc, sym->name);

  if (sym->attr.event_comp && coarray && !event_type)
    gfc_error ("Noncoarray component %s at %L of type EVENT_TYPE or with "
               "subcomponent of type EVENT_TYPE must have a codimension or "
               "be a subcomponent of a coarray. (Variables of type %s may "
               "not have a codimension as %s at %L has a codimension or a "
               "coarray subcomponent)", event_comp->name, &event_comp->loc,
               sym->name, c->name, &c->loc);

  /* Look for private components.  */
  if (sym->component_access == ACCESS_PRIVATE
      || c->attr.access == ACCESS_PRIVATE
      || (c->ts.type == BT_DERIVED && c->ts.u.derived->attr.private_comp))
    sym->attr.private_comp = 1;

  if (lockp) *lockp = lock_comp;
  if (eventp) *eventp = event_comp;
}


static void parse_struct_map (gfc_statement);

/* Parse a union component definition within a structure definition.  */

static void
parse_union (void)
{
  int compiling;
  gfc_statement st;
  gfc_state_data s;
  gfc_component *c, *lock_comp = NULL, *event_comp = NULL;
  gfc_symbol *un;

  accept_statement(ST_UNION);
  push_state (&s, COMP_UNION, gfc_new_block);
  un = gfc_new_block;

  compiling = 1;

  while (compiling)
    {
      st = next_statement ();
      /* Only MAP declarations valid within a union. */
      switch (st)
        {
        case ST_NONE:
          unexpected_eof ();

        case ST_MAP:
          accept_statement (ST_MAP);
          parse_struct_map (ST_MAP);
          /* Add a component to the union for each map. */
          if (!gfc_add_component (un, gfc_new_block->name, &c))
            {
              gfc_internal_error ("failed to create map component '%s'",
                  gfc_new_block->name);
              reject_statement ();
              return;
            }
          c->ts.type = BT_DERIVED;
          c->ts.u.derived = gfc_new_block;
          /* Normally components get their initialization expressions when they
             are created in decl.cc (build_struct) so we can look through the
             flat component list for initializers during resolution. Unions and
             maps create components along with their type definitions so we
             have to generate initializers here. */
          c->initializer = gfc_default_initializer (&c->ts);
          break;

        case ST_END_UNION:
          compiling = 0;
          accept_statement (ST_END_UNION);
          break;

        default:
          unexpected_statement (st);
          break;
        }
    }

  for (c = un->components; c; c = c->next)
    check_component (un, c, &lock_comp, &event_comp);

  /* Add the union as a component in its parent structure.  */
  pop_state ();
  if (!gfc_add_component (gfc_current_block (), un->name, &c))
    {
      gfc_internal_error ("failed to create union component '%s'", un->name);
      reject_statement ();
      return;
    }
  c->ts.type = BT_UNION;
  c->ts.u.derived = un;
  c->initializer = gfc_default_initializer (&c->ts);

  un->attr.zero_comp = un->components == NULL;
}


/* Parse a STRUCTURE or MAP.  */

static void
parse_struct_map (gfc_statement block)
{
  int compiling_type;
  gfc_statement st;
  gfc_state_data s;
  gfc_symbol *sym;
  gfc_component *c, *lock_comp = NULL, *event_comp = NULL;
  gfc_compile_state comp;
  gfc_statement ends;

  if (block == ST_STRUCTURE_DECL)
    {
      comp = COMP_STRUCTURE;
      ends = ST_END_STRUCTURE;
    }
  else
    {
      gcc_assert (block == ST_MAP);
      comp = COMP_MAP;
      ends = ST_END_MAP;
    }

  accept_statement(block);
  push_state (&s, comp, gfc_new_block);

  gfc_new_block->component_access = ACCESS_PUBLIC;
  compiling_type = 1;

  while (compiling_type)
    {
      st = next_statement ();
      switch (st)
        {
        case ST_NONE:
          unexpected_eof ();

        /* Nested structure declarations will be captured as ST_DATA_DECL.  */
        case ST_STRUCTURE_DECL:
          /* Let a more specific error make it to decode_statement().  */
          if (gfc_error_check () == 0)
            gfc_error ("Syntax error in nested structure declaration at %C");
          reject_statement ();
          /* Skip the rest of this statement.  */
          gfc_error_recovery ();
          break;

        case ST_UNION:
          accept_statement (ST_UNION);
          parse_union ();
          break;

        case ST_DATA_DECL:
          /* The data declaration was a nested/ad-hoc STRUCTURE field.  */
          accept_statement (ST_DATA_DECL);
          if (gfc_new_block && gfc_new_block != gfc_current_block ()
                            && gfc_new_block->attr.flavor == FL_STRUCT)
              parse_struct_map (ST_STRUCTURE_DECL);
          break;

        case ST_END_STRUCTURE:
        case ST_END_MAP:
          if (st == ends)
            {
              accept_statement (st);
              compiling_type = 0;
            }
          else
            unexpected_statement (st);
          break;

        default:
          unexpected_statement (st);
          break;
        }
    }

  /* Validate each component.  */
  sym = gfc_current_block ();
  for (c = sym->components; c; c = c->next)
    check_component (sym, c, &lock_comp, &event_comp);

  sym->attr.zero_comp = (sym->components == NULL);

  /* Allow parse_union to find this structure to add to its list of maps.  */
  if (block == ST_MAP)
    gfc_new_block = gfc_current_block ();

  pop_state ();
}


/* Parse a derived type.  */

static void
parse_derived (void)
{
  int compiling_type, seen_private, seen_sequence, seen_component;
  gfc_statement st;
  gfc_state_data s;
  gfc_symbol *sym;
  gfc_component *c, *lock_comp = NULL, *event_comp = NULL;

  accept_statement (ST_DERIVED_DECL);
  push_state (&s, COMP_DERIVED, gfc_new_block);

  gfc_new_block->component_access = ACCESS_PUBLIC;
  seen_private = 0;
  seen_sequence = 0;
  seen_component = 0;

  compiling_type = 1;

  while (compiling_type)
    {
      st = next_statement ();
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_DATA_DECL:
	case ST_PROCEDURE:
	  accept_statement (st);
	  seen_component = 1;
	  break;

	case ST_FINAL:
	  gfc_error ("FINAL declaration at %C must be inside CONTAINS");
	  break;

	case ST_END_TYPE:
endType:
	  compiling_type = 0;

	  if (!seen_component)
	    gfc_notify_std (GFC_STD_F2003, "Derived type "
			    "definition at %C without components");

	  accept_statement (ST_END_TYPE);
	  break;

	case ST_PRIVATE:
	  if (!gfc_find_state (COMP_MODULE))
	    {
	      gfc_error ("PRIVATE statement in TYPE at %C must be inside "
			 "a MODULE");
	      break;
	    }

	  if (seen_component)
	    {
	      gfc_error ("PRIVATE statement at %C must precede "
			 "structure components");
	      break;
	    }

	  if (seen_private)
	    gfc_error ("Duplicate PRIVATE statement at %C");

	  s.sym->component_access = ACCESS_PRIVATE;

	  accept_statement (ST_PRIVATE);
	  seen_private = 1;
	  break;

	case ST_SEQUENCE:
	  if (seen_component)
	    {
	      gfc_error ("SEQUENCE statement at %C must precede "
			 "structure components");
	      break;
	    }

	  if (gfc_current_block ()->attr.sequence)
	    gfc_warning (0, "SEQUENCE attribute at %C already specified in "
			 "TYPE statement");

	  if (seen_sequence)
	    {
	      gfc_error ("Duplicate SEQUENCE statement at %C");
	    }

	  seen_sequence = 1;
	  gfc_add_sequence (&gfc_current_block ()->attr,
			    gfc_current_block ()->name, NULL);
	  break;

	case ST_CONTAINS:
	  gfc_notify_std (GFC_STD_F2003,
			  "CONTAINS block in derived type"
			  " definition at %C");

	  accept_statement (ST_CONTAINS);
	  parse_derived_contains ();
	  goto endType;

	default:
	  unexpected_statement (st);
	  break;
	}
    }

  /* need to verify that all fields of the derived type are
   * interoperable with C if the type is declared to be bind(c)
   */
  sym = gfc_current_block ();
  for (c = sym->components; c; c = c->next)
    check_component (sym, c, &lock_comp, &event_comp);

  if (!seen_component)
    sym->attr.zero_comp = 1;

  pop_state ();
}


/* Parse an ENUM.  */

static void
parse_enum (void)
{
  gfc_statement st;
  int compiling_enum;
  gfc_state_data s;
  int seen_enumerator = 0;

  push_state (&s, COMP_ENUM, gfc_new_block);

  compiling_enum = 1;

  while (compiling_enum)
    {
      st = next_statement ();
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();
	  break;

	case ST_ENUMERATOR:
	  seen_enumerator = 1;
	  accept_statement (st);
	  break;

	case ST_END_ENUM:
	  compiling_enum = 0;
	  if (!seen_enumerator)
	    gfc_error ("ENUM declaration at %C has no ENUMERATORS");
	  accept_statement (st);
	  break;

	default:
	  gfc_free_enum_history ();
	  unexpected_statement (st);
	  break;
	}
    }
  pop_state ();
}


/* Parse an interface.  We must be able to deal with the possibility
   of recursive interfaces.  The parse_spec() subroutine is mutually
   recursive with parse_interface().  */

static gfc_statement parse_spec (gfc_statement);

static void
parse_interface (void)
{
  gfc_compile_state new_state = COMP_NONE, current_state;
  gfc_symbol *prog_unit, *sym;
  gfc_interface_info save;
  gfc_state_data s1, s2;
  gfc_statement st;

  accept_statement (ST_INTERFACE);

  current_interface.ns = gfc_current_ns;
  save = current_interface;

  sym = (current_interface.type == INTERFACE_GENERIC
	 || current_interface.type == INTERFACE_USER_OP)
	? gfc_new_block : NULL;

  push_state (&s1, COMP_INTERFACE, sym);
  current_state = COMP_NONE;

loop:
  gfc_current_ns = gfc_get_namespace (current_interface.ns, 0);

  st = next_statement ();
  switch (st)
    {
    case ST_NONE:
      unexpected_eof ();

    case ST_SUBROUTINE:
    case ST_FUNCTION:
      if (st == ST_SUBROUTINE)
	new_state = COMP_SUBROUTINE;
      else if (st == ST_FUNCTION)
	new_state = COMP_FUNCTION;
      if (gfc_new_block->attr.pointer)
	{
	  gfc_new_block->attr.pointer = 0;
	  gfc_new_block->attr.proc_pointer = 1;
	}
      if (!gfc_add_explicit_interface (gfc_new_block, IFSRC_IFBODY,
				       gfc_new_block->formal, NULL))
	{
	  reject_statement ();
	  gfc_free_namespace (gfc_current_ns);
	  goto loop;
	}
      /* F2008 C1210 forbids the IMPORT statement in module procedure
	 interface bodies and the flag is set to import symbols.  */
      if (gfc_new_block->attr.module_procedure)
        gfc_current_ns->has_import_set = 1;
      break;

    case ST_PROCEDURE:
    case ST_MODULE_PROC:	/* The module procedure matcher makes
				   sure the context is correct.  */
      accept_statement (st);
      gfc_free_namespace (gfc_current_ns);
      goto loop;

    case ST_END_INTERFACE:
      gfc_free_namespace (gfc_current_ns);
      gfc_current_ns = current_interface.ns;
      goto done;

    default:
      gfc_error ("Unexpected %s statement in INTERFACE block at %C",
		 gfc_ascii_statement (st));
      current_interface = save;
      reject_statement ();
      gfc_free_namespace (gfc_current_ns);
      goto loop;
    }


  /* Make sure that the generic name has the right attribute.  */
  if (current_interface.type == INTERFACE_GENERIC
      && current_state == COMP_NONE)
    {
      if (new_state == COMP_FUNCTION && sym)
	gfc_add_function (&sym->attr, sym->name, NULL);
      else if (new_state == COMP_SUBROUTINE && sym)
	gfc_add_subroutine (&sym->attr, sym->name, NULL);

      current_state = new_state;
    }

  if (current_interface.type == INTERFACE_ABSTRACT)
    {
      gfc_add_abstract (&gfc_new_block->attr, &gfc_current_locus);
      if (gfc_is_intrinsic_typename (gfc_new_block->name))
	gfc_error ("Name %qs of ABSTRACT INTERFACE at %C "
		   "cannot be the same as an intrinsic type",
		   gfc_new_block->name);
    }

  push_state (&s2, new_state, gfc_new_block);
  accept_statement (st);
  prog_unit = gfc_new_block;
  prog_unit->formal_ns = gfc_current_ns;

decl:
  /* Read data declaration statements.  */
  st = parse_spec (ST_NONE);
  in_specification_block = true;

  /* Since the interface block does not permit an IMPLICIT statement,
     the default type for the function or the result must be taken
     from the formal namespace.  */
  if (new_state == COMP_FUNCTION)
    {
	if (prog_unit->result == prog_unit
	      && prog_unit->ts.type == BT_UNKNOWN)
	  gfc_set_default_type (prog_unit, 1, prog_unit->formal_ns);
	else if (prog_unit->result != prog_unit
		   && prog_unit->result->ts.type == BT_UNKNOWN)
	  gfc_set_default_type (prog_unit->result, 1,
				prog_unit->formal_ns);
    }

  if (st != ST_END_SUBROUTINE && st != ST_END_FUNCTION)
    {
      gfc_error ("Unexpected %s statement at %C in INTERFACE body",
		 gfc_ascii_statement (st));
      reject_statement ();
      goto decl;
    }

  /* Add EXTERNAL attribute to function or subroutine.  */
  if (current_interface.type != INTERFACE_ABSTRACT && !prog_unit->attr.dummy)
    gfc_add_external (&prog_unit->attr, &gfc_current_locus);

  current_interface = save;
  gfc_add_interface (prog_unit);
  pop_state ();

  if (current_interface.ns
	&& current_interface.ns->proc_name
	&& strcmp (current_interface.ns->proc_name->name,
		   prog_unit->name) == 0)
    gfc_error ("INTERFACE procedure %qs at %L has the same name as the "
	       "enclosing procedure", prog_unit->name,
	       &current_interface.ns->proc_name->declared_at);

  goto loop;

done:
  pop_state ();
}


/* Associate function characteristics by going back to the function
   declaration and rematching the prefix.  */

static match
match_deferred_characteristics (gfc_typespec * ts)
{
  locus loc;
  match m = MATCH_ERROR;
  char name[GFC_MAX_SYMBOL_LEN + 1];

  loc = gfc_current_locus;

  gfc_current_locus = gfc_current_block ()->declared_at;

  gfc_clear_error ();
  gfc_buffer_error (true);
  m = gfc_match_prefix (ts);
  gfc_buffer_error (false);

  if (ts->type == BT_DERIVED || ts->type == BT_CLASS)
    {
      ts->kind = 0;

      if (!ts->u.derived)
	m = MATCH_ERROR;
    }

  /* Only permit one go at the characteristic association.  */
  if (ts->kind == -1)
    ts->kind = 0;

  /* Set the function locus correctly.  If we have not found the
     function name, there is an error.  */
  if (m == MATCH_YES
      && gfc_match ("function% %n", name) == MATCH_YES
      && strcmp (name, gfc_current_block ()->name) == 0)
    {
      gfc_current_block ()->declared_at = gfc_current_locus;
      gfc_commit_symbols ();
    }
  else
    {
      gfc_error_check ();
      gfc_undo_symbols ();
    }

  gfc_current_locus =loc;
  return m;
}


/* Check specification-expressions in the function result of the currently
   parsed block and ensure they are typed (give an IMPLICIT type if necessary).
   For return types specified in a FUNCTION prefix, the IMPLICIT rules of the
   scope are not yet parsed so this has to be delayed up to parse_spec.  */

static bool
check_function_result_typed (void)
{
  gfc_typespec ts;

  gcc_assert (gfc_current_state () == COMP_FUNCTION);

  if (!gfc_current_ns->proc_name->result)
    return true;

  ts = gfc_current_ns->proc_name->result->ts;

  /* Check type-parameters, at the moment only CHARACTER lengths possible.  */
  /* TODO:  Extend when KIND type parameters are implemented.  */
  if (ts.type == BT_CHARACTER && ts.u.cl && ts.u.cl->length)
    {
      /* Reject invalid type of specification expression for length.  */
      if (ts.u.cl->length->ts.type != BT_INTEGER)
	  return false;

      gfc_expr_check_typed (ts.u.cl->length, gfc_current_ns, true);
    }

  return true;
}


/* Parse a set of specification statements.  Returns the statement
   that doesn't fit.  */

static gfc_statement
parse_spec (gfc_statement st)
{
  st_state ss;
  bool function_result_typed = false;
  bool bad_characteristic = false;
  gfc_typespec *ts;

  in_specification_block = true;

  verify_st_order (&ss, ST_NONE, false);
  if (st == ST_NONE)
    st = next_statement ();

  /* If we are not inside a function or don't have a result specified so far,
     do nothing special about it.  */
  if (gfc_current_state () != COMP_FUNCTION)
    function_result_typed = true;
  else
    {
      gfc_symbol* proc = gfc_current_ns->proc_name;
      gcc_assert (proc);

      if (proc->result && proc->result->ts.type == BT_UNKNOWN)
	function_result_typed = true;
    }

loop:

  /* If we're inside a BLOCK construct, some statements are disallowed.
     Check this here.  Attribute declaration statements like INTENT, OPTIONAL
     or VALUE are also disallowed, but they don't have a particular ST_*
     key so we have to check for them individually in their matcher routine.  */
  if (gfc_current_state () == COMP_BLOCK)
    switch (st)
      {
	case ST_IMPLICIT:
	case ST_IMPLICIT_NONE:
	case ST_NAMELIST:
	case ST_COMMON:
	case ST_EQUIVALENCE:
	case ST_STATEMENT_FUNCTION:
	  gfc_error ("%s statement is not allowed inside of BLOCK at %C",
		     gfc_ascii_statement (st));
	  reject_statement ();
	  break;

	default:
	  break;
      }
  else if (gfc_current_state () == COMP_BLOCK_DATA)
    /* Fortran 2008, C1116.  */
    switch (st)
      {
	case ST_ATTR_DECL:
	case ST_COMMON:
	case ST_DATA:
	case ST_DATA_DECL:
	case ST_DERIVED_DECL:
	case ST_END_BLOCK_DATA:
	case ST_EQUIVALENCE:
	case ST_IMPLICIT:
	case ST_IMPLICIT_NONE:
	case ST_OMP_THREADPRIVATE:
	case ST_PARAMETER:
	case ST_STRUCTURE_DECL:
	case ST_TYPE:
	case ST_USE:
	  break;

	case ST_NONE:
	  break;

	default:
	  gfc_error ("%s statement is not allowed inside of BLOCK DATA at %C",
		     gfc_ascii_statement (st));
	  reject_statement ();
	  break;
      }

  /* If we find a statement that cannot be followed by an IMPLICIT statement
     (and thus we can expect to see none any further), type the function result
     if it has not yet been typed.  Be careful not to give the END statement
     to verify_st_order!  */
  if (!function_result_typed && st != ST_GET_FCN_CHARACTERISTICS)
    {
      bool verify_now = false;

      if (st == ST_END_FUNCTION || st == ST_CONTAINS)
	verify_now = true;
      else
	{
	  st_state dummyss;
	  verify_st_order (&dummyss, ST_NONE, false);
	  verify_st_order (&dummyss, st, false);

	  if (!verify_st_order (&dummyss, ST_IMPLICIT, true))
	    verify_now = true;
	}

      if (verify_now)
	function_result_typed = check_function_result_typed ();
    }

  switch (st)
    {
    case ST_NONE:
      unexpected_eof ();

    case ST_IMPLICIT_NONE:
    case ST_IMPLICIT:
      if (!function_result_typed)
	function_result_typed = check_function_result_typed ();
      goto declSt;

    case ST_FORMAT:
    case ST_ENTRY:
    case ST_DATA:	/* Not allowed in interfaces */
      if (gfc_current_state () == COMP_INTERFACE)
	break;

      /* Fall through */

    case ST_USE:
    case ST_IMPORT:
    case ST_PARAMETER:
    case ST_PUBLIC:
    case ST_PRIVATE:
    case ST_STRUCTURE_DECL:
    case ST_DERIVED_DECL:
    case_decl:
    case_omp_decl:
declSt:
      if (!verify_st_order (&ss, st, false))
	{
	  reject_statement ();
	  st = next_statement ();
	  goto loop;
	}

      switch (st)
	{
	case ST_INTERFACE:
	  parse_interface ();
	  break;

        case ST_STRUCTURE_DECL:
          parse_struct_map (ST_STRUCTURE_DECL);
          break;

	case ST_DERIVED_DECL:
	  parse_derived ();
	  break;

	case ST_PUBLIC:
	case ST_PRIVATE:
	  if (gfc_current_state () != COMP_MODULE)
	    {
	      gfc_error ("%s statement must appear in a MODULE",
			 gfc_ascii_statement (st));
	      reject_statement ();
	      break;
	    }

	  if (gfc_current_ns->default_access != ACCESS_UNKNOWN)
	    {
	      gfc_error ("%s statement at %C follows another accessibility "
			 "specification", gfc_ascii_statement (st));
	      reject_statement ();
	      break;
	    }

	  gfc_current_ns->default_access = (st == ST_PUBLIC)
	    ? ACCESS_PUBLIC : ACCESS_PRIVATE;

	  break;

	case ST_STATEMENT_FUNCTION:
	  if (gfc_current_state () == COMP_MODULE
	      || gfc_current_state () == COMP_SUBMODULE)
	    {
	      unexpected_statement (st);
	      break;
	    }

	default:
	  break;
	}

      accept_statement (st);
      st = next_statement ();
      goto loop;

    case ST_ENUM:
      accept_statement (st);
      parse_enum();
      st = next_statement ();
      goto loop;

    case ST_GET_FCN_CHARACTERISTICS:
      /* This statement triggers the association of a function's result
	 characteristics.  */
      ts = &gfc_current_block ()->result->ts;
      if (match_deferred_characteristics (ts) != MATCH_YES)
	bad_characteristic = true;

      st = next_statement ();
      goto loop;

    default:
      break;
    }

  /* If match_deferred_characteristics failed, then there is an error.  */
  if (bad_characteristic)
    {
      ts = &gfc_current_block ()->result->ts;
      if (ts->type != BT_DERIVED && ts->type != BT_CLASS)
	gfc_error ("Bad kind expression for function %qs at %L",
		   gfc_current_block ()->name,
		   &gfc_current_block ()->declared_at);
      else
	gfc_error ("The type for function %qs at %L is not accessible",
		   gfc_current_block ()->name,
		   &gfc_current_block ()->declared_at);

      gfc_current_block ()->ts.kind = 0;
      /* Keep the derived type; if it's bad, it will be discovered later.  */
      if (!(ts->type == BT_DERIVED && ts->u.derived))
	ts->type = BT_UNKNOWN;
    }

  in_specification_block = false;

  return st;
}


/* Parse a WHERE block, (not a simple WHERE statement).  */

static void
parse_where_block (void)
{
  int seen_empty_else;
  gfc_code *top, *d;
  gfc_state_data s;
  gfc_statement st;

  accept_statement (ST_WHERE_BLOCK);
  top = gfc_state_stack->tail;

  push_state (&s, COMP_WHERE, gfc_new_block);

  d = add_statement ();
  d->expr1 = top->expr1;
  d->op = EXEC_WHERE;

  top->expr1 = NULL;
  top->block = d;

  seen_empty_else = 0;

  do
    {
      st = next_statement ();
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_WHERE_BLOCK:
	  parse_where_block ();
	  break;

	case ST_ASSIGNMENT:
	case ST_WHERE:
	  accept_statement (st);
	  break;

	case ST_ELSEWHERE:
	  if (seen_empty_else)
	    {
	      gfc_error ("ELSEWHERE statement at %C follows previous "
			 "unmasked ELSEWHERE");
	      reject_statement ();
	      break;
	    }

	  if (new_st.expr1 == NULL)
	    seen_empty_else = 1;

	  d = new_level (gfc_state_stack->head);
	  d->op = EXEC_WHERE;
	  d->expr1 = new_st.expr1;

	  accept_statement (st);

	  break;

	case ST_END_WHERE:
	  accept_statement (st);
	  break;

	default:
	  gfc_error ("Unexpected %s statement in WHERE block at %C",
		     gfc_ascii_statement (st));
	  reject_statement ();
	  break;
	}
    }
  while (st != ST_END_WHERE);

  pop_state ();
}


/* Parse a FORALL block (not a simple FORALL statement).  */

static void
parse_forall_block (void)
{
  gfc_code *top, *d;
  gfc_state_data s;
  gfc_statement st;

  accept_statement (ST_FORALL_BLOCK);
  top = gfc_state_stack->tail;

  push_state (&s, COMP_FORALL, gfc_new_block);

  d = add_statement ();
  d->op = EXEC_FORALL;
  top->block = d;

  do
    {
      st = next_statement ();
      switch (st)
	{

	case ST_ASSIGNMENT:
	case ST_POINTER_ASSIGNMENT:
	case ST_WHERE:
	case ST_FORALL:
	  accept_statement (st);
	  break;

	case ST_WHERE_BLOCK:
	  parse_where_block ();
	  break;

	case ST_FORALL_BLOCK:
	  parse_forall_block ();
	  break;

	case ST_END_FORALL:
	  accept_statement (st);
	  break;

	case ST_NONE:
	  unexpected_eof ();

	default:
	  gfc_error ("Unexpected %s statement in FORALL block at %C",
		     gfc_ascii_statement (st));

	  reject_statement ();
	  break;
	}
    }
  while (st != ST_END_FORALL);

  pop_state ();
}


static gfc_statement parse_executable (gfc_statement);

/* parse the statements of an IF-THEN-ELSEIF-ELSE-ENDIF block.  */

static void
parse_if_block (void)
{
  gfc_code *top, *d;
  gfc_statement st;
  locus else_locus;
  gfc_state_data s;
  int seen_else;

  seen_else = 0;
  accept_statement (ST_IF_BLOCK);

  top = gfc_state_stack->tail;
  push_state (&s, COMP_IF, gfc_new_block);

  new_st.op = EXEC_IF;
  d = add_statement ();

  d->expr1 = top->expr1;
  top->expr1 = NULL;
  top->block = d;

  do
    {
      st = parse_executable (ST_NONE);

      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_ELSEIF:
	  if (seen_else)
	    {
	      gfc_error ("ELSE IF statement at %C cannot follow ELSE "
			 "statement at %L", &else_locus);

	      reject_statement ();
	      break;
	    }

	  d = new_level (gfc_state_stack->head);
	  d->op = EXEC_IF;
	  d->expr1 = new_st.expr1;

	  accept_statement (st);

	  break;

	case ST_ELSE:
	  if (seen_else)
	    {
	      gfc_error ("Duplicate ELSE statements at %L and %C",
			 &else_locus);
	      reject_statement ();
	      break;
	    }

	  seen_else = 1;
	  else_locus = gfc_current_locus;

	  d = new_level (gfc_state_stack->head);
	  d->op = EXEC_IF;

	  accept_statement (st);

	  break;

	case ST_ENDIF:
	  break;

	default:
	  unexpected_statement (st);
	  break;
	}
    }
  while (st != ST_ENDIF);

  pop_state ();
  accept_statement (st);
}


/* Parse a SELECT block.  */

static void
parse_select_block (void)
{
  gfc_statement st;
  gfc_code *cp;
  gfc_state_data s;

  accept_statement (ST_SELECT_CASE);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_SELECT, gfc_new_block);

  /* Make sure that the next statement is a CASE or END SELECT.  */
  for (;;)
    {
      st = next_statement ();
      if (st == ST_NONE)
	unexpected_eof ();
      if (st == ST_END_SELECT)
	{
	  /* Empty SELECT CASE is OK.  */
	  accept_statement (st);
	  pop_state ();
	  return;
	}
      if (st == ST_CASE)
	break;

      gfc_error ("Expected a CASE or END SELECT statement following SELECT "
		 "CASE at %C");

      reject_statement ();
    }

  /* At this point, we've got a nonempty select block.  */
  cp = new_level (cp);
  *cp = new_st;

  accept_statement (st);

  do
    {
      st = parse_executable (ST_NONE);
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_CASE:
	  cp = new_level (gfc_state_stack->head);
	  *cp = new_st;
	  gfc_clear_new_st ();

	  accept_statement (st);
	  /* Fall through */

	case ST_END_SELECT:
	  break;

	/* Can't have an executable statement because of
	   parse_executable().  */
	default:
	  unexpected_statement (st);
	  break;
	}
    }
  while (st != ST_END_SELECT);

  pop_state ();
  accept_statement (st);
}


/* Pop the current selector from the SELECT TYPE stack.  */

static void
select_type_pop (void)
{
  gfc_select_type_stack *old = select_type_stack;
  select_type_stack = old->prev;
  free (old);
}


/* Parse a SELECT TYPE construct (F03:R821).  */

static void
parse_select_type_block (void)
{
  gfc_statement st;
  gfc_code *cp;
  gfc_state_data s;

  gfc_current_ns = new_st.ext.block.ns;
  accept_statement (ST_SELECT_TYPE);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_SELECT_TYPE, gfc_new_block);

  /* Make sure that the next statement is a TYPE IS, CLASS IS, CLASS DEFAULT
     or END SELECT.  */
  for (;;)
    {
      st = next_statement ();
      if (st == ST_NONE)
	unexpected_eof ();
      if (st == ST_END_SELECT)
	/* Empty SELECT CASE is OK.  */
	goto done;
      if (st == ST_TYPE_IS || st == ST_CLASS_IS)
	break;

      gfc_error ("Expected TYPE IS, CLASS IS or END SELECT statement "
		 "following SELECT TYPE at %C");

      reject_statement ();
    }

  /* At this point, we've got a nonempty select block.  */
  cp = new_level (cp);
  *cp = new_st;

  accept_statement (st);

  do
    {
      st = parse_executable (ST_NONE);
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_TYPE_IS:
	case ST_CLASS_IS:
	  cp = new_level (gfc_state_stack->head);
	  *cp = new_st;
	  gfc_clear_new_st ();

	  accept_statement (st);
	  /* Fall through */

	case ST_END_SELECT:
	  break;

	/* Can't have an executable statement because of
	   parse_executable().  */
	default:
	  unexpected_statement (st);
	  break;
	}
    }
  while (st != ST_END_SELECT);

done:
  pop_state ();
  accept_statement (st);
  gfc_current_ns = gfc_current_ns->parent;
  select_type_pop ();
}


/* Parse a SELECT RANK construct.  */

static void
parse_select_rank_block (void)
{
  gfc_statement st;
  gfc_code *cp;
  gfc_state_data s;

  gfc_current_ns = new_st.ext.block.ns;
  accept_statement (ST_SELECT_RANK);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_SELECT_RANK, gfc_new_block);

  /* Make sure that the next statement is a RANK IS or RANK DEFAULT.  */
  for (;;)
    {
      st = next_statement ();
      if (st == ST_NONE)
	unexpected_eof ();
      if (st == ST_END_SELECT)
	/* Empty SELECT CASE is OK.  */
	goto done;
      if (st == ST_RANK)
	break;

      gfc_error ("Expected RANK or RANK DEFAULT "
		 "following SELECT RANK at %C");

      reject_statement ();
    }

  /* At this point, we've got a nonempty select block.  */
  cp = new_level (cp);
  *cp = new_st;

  accept_statement (st);

  do
    {
      st = parse_executable (ST_NONE);
      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_RANK:
	  cp = new_level (gfc_state_stack->head);
	  *cp = new_st;
	  gfc_clear_new_st ();

	  accept_statement (st);
	  /* Fall through */

	case ST_END_SELECT:
	  break;

	/* Can't have an executable statement because of
	   parse_executable().  */
	default:
	  unexpected_statement (st);
	  break;
	}
    }
  while (st != ST_END_SELECT);

done:
  pop_state ();
  accept_statement (st);
  gfc_current_ns = gfc_current_ns->parent;
  select_type_pop ();
}


/* Given a symbol, make sure it is not an iteration variable for a DO
   statement.  This subroutine is called when the symbol is seen in a
   context that causes it to become redefined.  If the symbol is an
   iterator, we generate an error message and return nonzero.  */

bool
gfc_check_do_variable (gfc_symtree *st)
{
  gfc_state_data *s;

  if (!st)
    return 0;

  for (s=gfc_state_stack; s; s = s->previous)
    if (s->do_variable == st)
      {
	gfc_error_now ("Variable %qs at %C cannot be redefined inside "
		       "loop beginning at %L", st->name, &s->head->loc);
	return 1;
      }

  return 0;
}


/* Checks to see if the current statement label closes an enddo.
   Returns 0 if not, 1 if closes an ENDDO correctly, or 2 (and issues
   an error) if it incorrectly closes an ENDDO.  */

static int
check_do_closure (void)
{
  gfc_state_data *p;

  if (gfc_statement_label == NULL)
    return 0;

  for (p = gfc_state_stack; p; p = p->previous)
    if (p->state == COMP_DO || p->state == COMP_DO_CONCURRENT)
      break;

  if (p == NULL)
    return 0;		/* No loops to close */

  if (p->ext.end_do_label == gfc_statement_label)
    {
      if (p == gfc_state_stack)
	return 1;

      gfc_error ("End of nonblock DO statement at %C is within another block");
      return 2;
    }

  /* At this point, the label doesn't terminate the innermost loop.
     Make sure it doesn't terminate another one.  */
  for (; p; p = p->previous)
    if ((p->state == COMP_DO || p->state == COMP_DO_CONCURRENT)
	&& p->ext.end_do_label == gfc_statement_label)
      {
	gfc_error ("End of nonblock DO statement at %C is interwoven "
		   "with another DO loop");
	return 2;
      }

  return 0;
}


/* Parse a series of contained program units.  */

static void parse_progunit (gfc_statement);


/* Parse a CRITICAL block.  */

static void
parse_critical_block (void)
{
  gfc_code *top, *d;
  gfc_state_data s, *sd;
  gfc_statement st;

  for (sd = gfc_state_stack; sd; sd = sd->previous)
    if (sd->state == COMP_OMP_STRUCTURED_BLOCK)
      gfc_error_now (is_oacc (sd)
		     ? G_("CRITICAL block inside of OpenACC region at %C")
		     : G_("CRITICAL block inside of OpenMP region at %C"));

  s.ext.end_do_label = new_st.label1;

  accept_statement (ST_CRITICAL);
  top = gfc_state_stack->tail;

  push_state (&s, COMP_CRITICAL, gfc_new_block);

  d = add_statement ();
  d->op = EXEC_CRITICAL;
  top->block = d;

  do
    {
      st = parse_executable (ST_NONE);

      switch (st)
	{
	  case ST_NONE:
	    unexpected_eof ();
	    break;

	  case ST_END_CRITICAL:
	    if (s.ext.end_do_label != NULL
		&& s.ext.end_do_label != gfc_statement_label)
	      gfc_error_now ("Statement label in END CRITICAL at %C does not "
			     "match CRITICAL label");

	    if (gfc_statement_label != NULL)
	      {
		new_st.op = EXEC_NOP;
		add_statement ();
	      }
	    break;

	  default:
	    unexpected_statement (st);
	    break;
	}
    }
  while (st != ST_END_CRITICAL);

  pop_state ();
  accept_statement (st);
}


/* Set up the local namespace for a BLOCK construct.  */

gfc_namespace*
gfc_build_block_ns (gfc_namespace *parent_ns)
{
  gfc_namespace* my_ns;
  static int numblock = 1;

  my_ns = gfc_get_namespace (parent_ns, 1);
  my_ns->construct_entities = 1;

  /* Give the BLOCK a symbol of flavor LABEL; this is later needed for correct
     code generation (so it must not be NULL).
     We set its recursive argument if our container procedure is recursive, so
     that local variables are accordingly placed on the stack when it
     will be necessary.  */
  if (gfc_new_block)
    my_ns->proc_name = gfc_new_block;
  else
    {
      bool t;
      char buffer[20];  /* Enough to hold "block@2147483648\n".  */

      snprintf(buffer, sizeof(buffer), "block@%d", numblock++);
      gfc_get_symbol (buffer, my_ns, &my_ns->proc_name);
      t = gfc_add_flavor (&my_ns->proc_name->attr, FL_LABEL,
			  my_ns->proc_name->name, NULL);
      gcc_assert (t);
      gfc_commit_symbol (my_ns->proc_name);
    }

  if (parent_ns->proc_name)
    my_ns->proc_name->attr.recursive = parent_ns->proc_name->attr.recursive;

  return my_ns;
}


/* Parse a BLOCK construct.  */

static void
parse_block_construct (void)
{
  gfc_namespace* my_ns;
  gfc_namespace* my_parent;
  gfc_state_data s;

  gfc_notify_std (GFC_STD_F2008, "BLOCK construct at %C");

  my_ns = gfc_build_block_ns (gfc_current_ns);

  new_st.op = EXEC_BLOCK;
  new_st.ext.block.ns = my_ns;
  new_st.ext.block.assoc = NULL;
  accept_statement (ST_BLOCK);

  push_state (&s, COMP_BLOCK, my_ns->proc_name);
  gfc_current_ns = my_ns;
  my_parent = my_ns->parent;

  parse_progunit (ST_NONE);

  /* Don't depend on the value of gfc_current_ns;  it might have been
     reset if the block had errors and was cleaned up.  */
  gfc_current_ns = my_parent;

  pop_state ();
}


/* Parse an ASSOCIATE construct.  This is essentially a BLOCK construct
   behind the scenes with compiler-generated variables.  */

static void
parse_associate (void)
{
  gfc_namespace* my_ns;
  gfc_state_data s;
  gfc_statement st;
  gfc_association_list* a;
  gfc_array_spec *as;

  gfc_notify_std (GFC_STD_F2003, "ASSOCIATE construct at %C");

  my_ns = gfc_build_block_ns (gfc_current_ns);

  new_st.op = EXEC_BLOCK;
  new_st.ext.block.ns = my_ns;
  gcc_assert (new_st.ext.block.assoc);

  /* Add all associate-names as BLOCK variables.  Creating them is enough
     for now, they'll get their values during trans-* phase.  */
  gfc_current_ns = my_ns;
  for (a = new_st.ext.block.assoc; a; a = a->next)
    {
      gfc_symbol *sym, *tsym;
      gfc_expr *target;
      int rank;

      if (gfc_get_sym_tree (a->name, NULL, &a->st, false))
	gcc_unreachable ();

      sym = a->st->n.sym;
      sym->attr.flavor = FL_VARIABLE;
      sym->assoc = a;
      sym->declared_at = a->where;
      gfc_set_sym_referenced (sym);

      /* If the selector is a inferred type then the associate_name had better
	 be as well. Use array references, if present, to identify it as an
	 array.  */
      if (IS_INFERRED_TYPE (a->target))
	{
	  sym->assoc->inferred_type = 1;
	  for (gfc_ref *r = a->target->ref; r; r = r->next)
	    if (r->type == REF_ARRAY)
	      sym->attr.dimension = 1;
	}

      /* Initialize the typespec.  It is not available in all cases,
	 however, as it may only be set on the target during resolution.
	 Still, sometimes it helps to have it right now -- especially
	 for parsing component references on the associate-name
	 in case of association to a derived-type.  */
      sym->ts = a->target->ts;
      target = a->target;

      /* Don’t share the character length information between associate
	 variable and target if the length is not a compile-time constant,
	 as we don’t want to touch some other character length variable when
	 we try to initialize the associate variable’s character length
	 variable.
	 We do it here rather than later so that expressions referencing the
	 associate variable will automatically have the correctly setup length
	 information.  If we did it at resolution stage the expressions would
	 use the original length information, and the variable a new different
	 one, but only the latter one would be correctly initialized at
	 translation stage, and the former one would need some additional setup
	 there.  */
      if (sym->ts.type == BT_CHARACTER
	  && sym->ts.u.cl
	  && !(sym->ts.u.cl->length
	       && sym->ts.u.cl->length->expr_type == EXPR_CONSTANT))
	sym->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

      /* If the function has been parsed, go straight to the result to
	 obtain the expression rank.  */
      if (target->expr_type == EXPR_FUNCTION
	  && target->symtree
	  && target->symtree->n.sym)
	{
	  tsym = target->symtree->n.sym;
	  if (!tsym->result)
	    tsym->result = tsym;
	  sym->ts = tsym->result->ts;
	  if (sym->ts.type == BT_CLASS)
	    {
	      if (CLASS_DATA (sym)->as)
		target->rank = CLASS_DATA (sym)->as->rank;
	      sym->attr.class_ok = 1;
	    }
	  else
	    target->rank = tsym->result->as ? tsym->result->as->rank : 0;
	}

      /* Check if the target expression is array valued. This cannot be done
	 by calling gfc_resolve_expr because the context is unavailable.
	 However, the references can be resolved and the rank of the target
	 expression set.  */
      if (!sym->assoc->inferred_type
	  && target->ref && gfc_resolve_ref (target)
	  && target->expr_type != EXPR_ARRAY
	  && target->expr_type != EXPR_COMPCALL)
	gfc_expression_rank (target);

      /* Determine whether or not function expressions with unknown type are
	 structure constructors. If so, the function result can be converted
	 to be a derived type.  */
      if (target->expr_type == EXPR_FUNCTION
	  && target->ts.type == BT_UNKNOWN)
	{
	  gfc_symbol *derived;
	  /* The derived type has a leading uppercase character.  */
	  gfc_find_symbol (gfc_dt_upper_string (target->symtree->name),
			   my_ns->parent, 1, &derived);
	  if (derived && derived->attr.flavor == FL_DERIVED)
	    {
	      sym->ts.type = BT_DERIVED;
	      sym->ts.u.derived = derived;
	      sym->assoc->inferred_type = 0;
	    }
	}

      rank = target->rank;
      /* Fixup cases where the ranks are mismatched.  */
      if (sym->ts.type == BT_CLASS && CLASS_DATA (sym))
	{
	  if ((!CLASS_DATA (sym)->as && rank != 0)
	       || (CLASS_DATA (sym)->as
		   && CLASS_DATA (sym)->as->rank != rank))
	    {
	      /* Don't just (re-)set the attr and as in the sym.ts,
	      because this modifies the target's attr and as.  Copy the
	      data and do a build_class_symbol.  */
	      symbol_attribute attr = CLASS_DATA (target)->attr;
	      int corank = gfc_get_corank (target);
	      gfc_typespec type;

	      if (rank || corank)
		{
		  as = gfc_get_array_spec ();
		  as->type = AS_DEFERRED;
		  as->rank = rank;
		  as->corank = corank;
		  attr.dimension = rank ? 1 : 0;
		  attr.codimension = corank ? 1 : 0;
		}
	      else
		{
		  as = NULL;
		  attr.dimension = attr.codimension = 0;
		}
	      attr.class_ok = 0;
	      type = CLASS_DATA (sym)->ts;
	      if (!gfc_build_class_symbol (&type, &attr, &as))
		gcc_unreachable ();
	      sym->ts = type;
	      sym->ts.type = BT_CLASS;
	      sym->attr.class_ok = 1;
	    }
	  else
	    sym->attr.class_ok = 1;
	}
      else if ((!sym->as && rank != 0)
	       || (sym->as && sym->as->rank != rank))
	{
	  as = gfc_get_array_spec ();
	  as->type = AS_DEFERRED;
	  as->rank = rank;
	  as->corank = gfc_get_corank (target);
	  sym->as = as;
	  sym->attr.dimension = 1;
	  if (as->corank)
	    sym->attr.codimension = 1;
	}
    }

  accept_statement (ST_ASSOCIATE);
  push_state (&s, COMP_ASSOCIATE, my_ns->proc_name);

loop:
  st = parse_executable (ST_NONE);
  switch (st)
    {
    case ST_NONE:
      unexpected_eof ();

    case_end:
      accept_statement (st);
      my_ns->code = gfc_state_stack->head;
      break;

    default:
      unexpected_statement (st);
      goto loop;
    }

  gfc_current_ns = gfc_current_ns->parent;
  pop_state ();
}


/* Parse a DO loop.  Note that the ST_CYCLE and ST_EXIT statements are
   handled inside of parse_executable(), because they aren't really
   loop statements.  */

static void
parse_do_block (void)
{
  gfc_statement st;
  gfc_code *top;
  gfc_state_data s;
  gfc_symtree *stree;
  gfc_exec_op do_op;

  do_op = new_st.op;
  s.ext.end_do_label = new_st.label1;

  if (do_op == EXEC_DO_CONCURRENT)
    {
      gfc_forall_iterator *fa;
      for (fa = new_st.ext.forall_iterator; fa; fa = fa->next)
	{
	  /* Apply unroll only to innermost loop (first control
	     variable).  */
	  if (directive_unroll != -1)
	    {
	      fa->annot.unroll = directive_unroll;
	      directive_unroll = -1;
	    }
	  if (directive_ivdep)
	    fa->annot.ivdep = directive_ivdep;
	  if (directive_vector)
	    fa->annot.vector = directive_vector;
	  if (directive_novector)
	    fa->annot.novector = directive_novector;
	}
      directive_ivdep = false;
      directive_vector = false;
      directive_novector = false;
      stree = NULL;
    }
  else if (new_st.ext.iterator != NULL)
    {
      stree = new_st.ext.iterator->var->symtree;
      if (directive_unroll != -1)
	{
	  new_st.ext.iterator->annot.unroll = directive_unroll;
	  directive_unroll = -1;
	}
      if (directive_ivdep)
	{
	  new_st.ext.iterator->annot.ivdep = directive_ivdep;
	  directive_ivdep = false;
	}
      if (directive_vector)
	{
	  new_st.ext.iterator->annot.vector = directive_vector;
	  directive_vector = false;
	}
      if (directive_novector)
	{
	  new_st.ext.iterator->annot.novector = directive_novector;
	  directive_novector = false;
	}
    }
  else
    stree = NULL;

  accept_statement (ST_DO);

  top = gfc_state_stack->tail;
  push_state (&s, do_op == EXEC_DO_CONCURRENT ? COMP_DO_CONCURRENT : COMP_DO,
	      gfc_new_block);

  s.do_variable = stree;

  top->block = new_level (top);
  top->block->op = EXEC_DO;

loop:
  st = parse_executable (ST_NONE);

  switch (st)
    {
    case ST_NONE:
      unexpected_eof ();

    case ST_ENDDO:
      if (s.ext.end_do_label != NULL
	  && s.ext.end_do_label != gfc_statement_label)
	gfc_error_now ("Statement label in ENDDO at %C doesn't match "
		       "DO label");

      if (gfc_statement_label != NULL)
	{
	  new_st.op = EXEC_NOP;
	  add_statement ();
	}
      break;

    case ST_IMPLIED_ENDDO:
     /* If the do-stmt of this DO construct has a do-construct-name,
	the corresponding end-do must be an end-do-stmt (with a matching
	name, but in that case we must have seen ST_ENDDO first).
	We only complain about this in pedantic mode.  */
     if (gfc_current_block () != NULL)
	gfc_error_now ("Named block DO at %L requires matching ENDDO name",
		       &gfc_current_block()->declared_at);

      break;

    default:
      unexpected_statement (st);
      goto loop;
    }

  pop_state ();
  accept_statement (st);
}


/* Parse the statements of OpenMP do/parallel do.  */

static gfc_statement
parse_omp_do (gfc_statement omp_st)
{
  gfc_statement st;
  gfc_code *cp, *np;
  gfc_state_data s;

  accept_statement (omp_st);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_OMP_STRUCTURED_BLOCK, NULL);
  np = new_level (cp);
  np->op = cp->op;
  np->block = NULL;

  for (;;)
    {
      st = next_statement ();
      if (st == ST_NONE)
	unexpected_eof ();
      else if (st == ST_DO)
	break;
      else
	unexpected_statement (st);
    }

  parse_do_block ();
  if (gfc_statement_label != NULL
      && gfc_state_stack->previous != NULL
      && gfc_state_stack->previous->state == COMP_DO
      && gfc_state_stack->previous->ext.end_do_label == gfc_statement_label)
    {
      /* In
	 DO 100 I=1,10
	   !$OMP DO
	     DO J=1,10
	     ...
	     100 CONTINUE
	 there should be no !$OMP END DO.  */
      pop_state ();
      return ST_IMPLIED_ENDDO;
    }

  check_do_closure ();
  pop_state ();

  st = next_statement ();
  gfc_statement omp_end_st = ST_OMP_END_DO;
  switch (omp_st)
    {
    case ST_OMP_DISTRIBUTE: omp_end_st = ST_OMP_END_DISTRIBUTE; break;
    case ST_OMP_DISTRIBUTE_PARALLEL_DO:
      omp_end_st = ST_OMP_END_DISTRIBUTE_PARALLEL_DO;
      break;
    case ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      omp_end_st = ST_OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD;
      break;
    case ST_OMP_DISTRIBUTE_SIMD:
      omp_end_st = ST_OMP_END_DISTRIBUTE_SIMD;
      break;
    case ST_OMP_DO: omp_end_st = ST_OMP_END_DO; break;
    case ST_OMP_DO_SIMD: omp_end_st = ST_OMP_END_DO_SIMD; break;
    case ST_OMP_LOOP: omp_end_st = ST_OMP_END_LOOP; break;
    case ST_OMP_PARALLEL_DO: omp_end_st = ST_OMP_END_PARALLEL_DO; break;
    case ST_OMP_PARALLEL_DO_SIMD:
      omp_end_st = ST_OMP_END_PARALLEL_DO_SIMD;
      break;
    case ST_OMP_PARALLEL_LOOP:
      omp_end_st = ST_OMP_END_PARALLEL_LOOP;
      break;
    case ST_OMP_SIMD: omp_end_st = ST_OMP_END_SIMD; break;
    case ST_OMP_TARGET_PARALLEL_DO:
      omp_end_st = ST_OMP_END_TARGET_PARALLEL_DO;
      break;
    case ST_OMP_TARGET_PARALLEL_DO_SIMD:
      omp_end_st = ST_OMP_END_TARGET_PARALLEL_DO_SIMD;
      break;
    case ST_OMP_TARGET_PARALLEL_LOOP:
      omp_end_st = ST_OMP_END_TARGET_PARALLEL_LOOP;
      break;
    case ST_OMP_TARGET_SIMD: omp_end_st = ST_OMP_END_TARGET_SIMD; break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE:
      omp_end_st = ST_OMP_END_TARGET_TEAMS_DISTRIBUTE;
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      omp_end_st = ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO;
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      omp_end_st = ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
      break;
    case ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
      omp_end_st = ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD;
      break;
    case ST_OMP_TARGET_TEAMS_LOOP:
      omp_end_st = ST_OMP_END_TARGET_TEAMS_LOOP;
      break;
    case ST_OMP_TASKLOOP: omp_end_st = ST_OMP_END_TASKLOOP; break;
    case ST_OMP_TASKLOOP_SIMD: omp_end_st = ST_OMP_END_TASKLOOP_SIMD; break;
    case ST_OMP_MASKED_TASKLOOP: omp_end_st = ST_OMP_END_MASKED_TASKLOOP; break;
    case ST_OMP_MASKED_TASKLOOP_SIMD:
      omp_end_st = ST_OMP_END_MASKED_TASKLOOP_SIMD;
      break;
    case ST_OMP_MASTER_TASKLOOP: omp_end_st = ST_OMP_END_MASTER_TASKLOOP; break;
    case ST_OMP_MASTER_TASKLOOP_SIMD:
      omp_end_st = ST_OMP_END_MASTER_TASKLOOP_SIMD;
      break;
    case ST_OMP_PARALLEL_MASKED_TASKLOOP:
      omp_end_st = ST_OMP_END_PARALLEL_MASKED_TASKLOOP;
      break;
    case ST_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
      omp_end_st = ST_OMP_END_PARALLEL_MASKED_TASKLOOP_SIMD;
      break;
    case ST_OMP_PARALLEL_MASTER_TASKLOOP:
      omp_end_st = ST_OMP_END_PARALLEL_MASTER_TASKLOOP;
      break;
    case ST_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      omp_end_st = ST_OMP_END_PARALLEL_MASTER_TASKLOOP_SIMD;
      break;
    case ST_OMP_TEAMS_DISTRIBUTE:
      omp_end_st = ST_OMP_END_TEAMS_DISTRIBUTE;
      break;
    case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      omp_end_st = ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO;
      break;
    case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      omp_end_st = ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
      break;
    case ST_OMP_TEAMS_DISTRIBUTE_SIMD:
      omp_end_st = ST_OMP_END_TEAMS_DISTRIBUTE_SIMD;
      break;
    case ST_OMP_TEAMS_LOOP:
      omp_end_st = ST_OMP_END_TEAMS_LOOP;
      break;
    default: gcc_unreachable ();
    }
  if (st == omp_end_st)
    {
      if (new_st.op == EXEC_OMP_END_NOWAIT)
	{
	  if (cp->ext.omp_clauses->nowait && new_st.ext.omp_bool)
	    gfc_error_now ("Duplicated NOWAIT clause on %s and %s at %C",
			   gfc_ascii_statement (omp_st),
			   gfc_ascii_statement (omp_end_st));
	  cp->ext.omp_clauses->nowait |= new_st.ext.omp_bool;
	}
      else
	gcc_assert (new_st.op == EXEC_NOP);
      gfc_clear_new_st ();
      gfc_commit_symbols ();
      gfc_warning_check ();
      st = next_statement ();
    }
  return st;
}


/* Parse the statements of OpenMP atomic directive.  */

static gfc_statement
parse_omp_oacc_atomic (bool omp_p)
{
  gfc_statement st, st_atomic, st_end_atomic;
  gfc_code *cp, *np;
  gfc_state_data s;
  int count;

  if (omp_p)
    {
      st_atomic = ST_OMP_ATOMIC;
      st_end_atomic = ST_OMP_END_ATOMIC;
    }
  else
    {
      st_atomic = ST_OACC_ATOMIC;
      st_end_atomic = ST_OACC_END_ATOMIC;
    }
  accept_statement (st_atomic);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_OMP_STRUCTURED_BLOCK, NULL);
  np = new_level (cp);
  np->op = cp->op;
  np->block = NULL;
  np->ext.omp_clauses = cp->ext.omp_clauses;
  cp->ext.omp_clauses = NULL;
  count = 1 + np->ext.omp_clauses->capture;

  while (count)
    {
      st = next_statement ();
      if (st == ST_NONE)
	unexpected_eof ();
      else if (np->ext.omp_clauses->compare
	       && (st == ST_SIMPLE_IF || st == ST_IF_BLOCK))
	{
	  count--;
	  if (st == ST_IF_BLOCK)
	    {
	      parse_if_block ();
	      /* With else (or elseif).  */
	      if (gfc_state_stack->tail->block->block)
		count--;
	    }
	  accept_statement (st);
	}
      else if (st == ST_ASSIGNMENT
	       && (!np->ext.omp_clauses->compare
		   || np->ext.omp_clauses->capture))
	{
	  accept_statement (st);
	  count--;
	}
      else
	unexpected_statement (st);
    }

  pop_state ();

  st = next_statement ();
  if (st == st_end_atomic)
    {
      gfc_clear_new_st ();
      gfc_commit_symbols ();
      gfc_warning_check ();
      st = next_statement ();
    }
  return st;
}


/* Parse the statements of an OpenACC structured block.  */

static void
parse_oacc_structured_block (gfc_statement acc_st)
{
  gfc_statement st, acc_end_st;
  gfc_code *cp, *np;
  gfc_state_data s, *sd;

  for (sd = gfc_state_stack; sd; sd = sd->previous)
    if (sd->state == COMP_CRITICAL)
      gfc_error_now ("OpenACC directive inside of CRITICAL block at %C");

  accept_statement (acc_st);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_OMP_STRUCTURED_BLOCK, NULL);
  np = new_level (cp);
  np->op = cp->op;
  np->block = NULL;
  switch (acc_st)
    {
    case ST_OACC_PARALLEL:
      acc_end_st = ST_OACC_END_PARALLEL;
      break;
    case ST_OACC_KERNELS:
      acc_end_st = ST_OACC_END_KERNELS;
      break;
    case ST_OACC_SERIAL:
      acc_end_st = ST_OACC_END_SERIAL;
      break;
    case ST_OACC_DATA:
      acc_end_st = ST_OACC_END_DATA;
      break;
    case ST_OACC_HOST_DATA:
      acc_end_st = ST_OACC_END_HOST_DATA;
      break;
    default:
      gcc_unreachable ();
    }

  do
    {
      st = parse_executable (ST_NONE);
      if (st == ST_NONE)
	unexpected_eof ();
      else if (st != acc_end_st)
	{
	  gfc_error ("Expecting %s at %C", gfc_ascii_statement (acc_end_st));
	  reject_statement ();
	}
    }
  while (st != acc_end_st);

  gcc_assert (new_st.op == EXEC_NOP);

  gfc_clear_new_st ();
  gfc_commit_symbols ();
  gfc_warning_check ();
  pop_state ();
}

/* Parse the statements of OpenACC 'loop', or combined compute 'loop'.  */

static gfc_statement
parse_oacc_loop (gfc_statement acc_st)
{
  gfc_statement st;
  gfc_code *cp, *np;
  gfc_state_data s, *sd;

  for (sd = gfc_state_stack; sd; sd = sd->previous)
    if (sd->state == COMP_CRITICAL)
      gfc_error_now ("OpenACC directive inside of CRITICAL block at %C");

  accept_statement (acc_st);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_OMP_STRUCTURED_BLOCK, NULL);
  np = new_level (cp);
  np->op = cp->op;
  np->block = NULL;

  for (;;)
    {
      st = next_statement ();
      if (st == ST_NONE)
	unexpected_eof ();
      else if (st == ST_DO)
	break;
      else
	{
	  gfc_error ("Expected DO loop at %C");
	  reject_statement ();
	}
    }

  parse_do_block ();
  if (gfc_statement_label != NULL
      && gfc_state_stack->previous != NULL
      && gfc_state_stack->previous->state == COMP_DO
      && gfc_state_stack->previous->ext.end_do_label == gfc_statement_label)
    {
      pop_state ();
      return ST_IMPLIED_ENDDO;
    }

  check_do_closure ();
  pop_state ();

  st = next_statement ();
  if (st == ST_OACC_END_LOOP)
    gfc_warning (0, "Redundant !$ACC END LOOP at %C");
  if ((acc_st == ST_OACC_PARALLEL_LOOP && st == ST_OACC_END_PARALLEL_LOOP) ||
      (acc_st == ST_OACC_KERNELS_LOOP && st == ST_OACC_END_KERNELS_LOOP) ||
      (acc_st == ST_OACC_SERIAL_LOOP && st == ST_OACC_END_SERIAL_LOOP) ||
      (acc_st == ST_OACC_LOOP && st == ST_OACC_END_LOOP))
    {
      gcc_assert (new_st.op == EXEC_NOP);
      gfc_clear_new_st ();
      gfc_commit_symbols ();
      gfc_warning_check ();
      st = next_statement ();
    }
  return st;
}


/* Parse an OpenMP allocate block, including optional ALLOCATORS
   end directive.  */

static gfc_statement
parse_openmp_allocate_block (gfc_statement omp_st)
{
  gfc_statement st;
  gfc_code *cp, *np;
  gfc_state_data s;
  bool empty_list = false;
  locus empty_list_loc;
  gfc_omp_namelist *n_first = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE];

  if (omp_st == ST_OMP_ALLOCATE_EXEC
      && new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->sym == NULL)
    {
      empty_list = true;
      empty_list_loc = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->where;
    }

  accept_statement (omp_st);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_OMP_STRUCTURED_BLOCK, NULL);
  np = new_level (cp);
  np->op = cp->op;
  np->block = NULL;

  st = next_statement ();
  while (omp_st == ST_OMP_ALLOCATE_EXEC && st == ST_OMP_ALLOCATE_EXEC)
    {
      if (empty_list && !new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->sym)
	{
	  locus *loc = &new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->where;
	  gfc_error_now ("%s statements at %L and %L have both no list item but"
			 " only one may", gfc_ascii_statement (st),
			 &empty_list_loc, loc);
	  empty_list = false;
	}
      if (!new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->sym)
	{
	  empty_list = true;
	  empty_list_loc = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE]->where;
	}
      for ( ; n_first->next; n_first = n_first->next)
	;
      n_first->next = new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE];
      new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE] = NULL;
      gfc_free_omp_clauses (new_st.ext.omp_clauses);

      accept_statement (ST_NONE);
      st = next_statement ();
    }
  if (st != ST_ALLOCATE && omp_st == ST_OMP_ALLOCATE_EXEC)
    gfc_error_now ("Unexpected %s at %C; expected ALLOCATE or %s statement",
		   gfc_ascii_statement (st), gfc_ascii_statement (omp_st));
  else if (st != ST_ALLOCATE)
    gfc_error_now ("Unexpected %s at %C; expected ALLOCATE statement after %s",
		   gfc_ascii_statement (st), gfc_ascii_statement (omp_st));
  accept_statement (st);
  pop_state ();
  st = next_statement ();
  if (omp_st == ST_OMP_ALLOCATORS && st == ST_OMP_END_ALLOCATORS)
    {
      accept_statement (st);
      st = next_statement ();
    }
  return st;
}


/* Parse the statements of an OpenMP structured block.  */

static gfc_statement
parse_omp_structured_block (gfc_statement omp_st, bool workshare_stmts_only)
{
  gfc_statement st, omp_end_st, first_st;
  gfc_code *cp, *np;
  gfc_state_data s, s2;

  accept_statement (omp_st);

  cp = gfc_state_stack->tail;
  push_state (&s, COMP_OMP_STRUCTURED_BLOCK, NULL);
  np = new_level (cp);
  np->op = cp->op;
  np->block = NULL;

  switch (omp_st)
    {
    case ST_OMP_ASSUME:
      omp_end_st = ST_OMP_END_ASSUME;
      break;
    case ST_OMP_PARALLEL:
      omp_end_st = ST_OMP_END_PARALLEL;
      break;
    case ST_OMP_PARALLEL_MASKED:
      omp_end_st = ST_OMP_END_PARALLEL_MASKED;
      break;
    case ST_OMP_PARALLEL_MASTER:
      omp_end_st = ST_OMP_END_PARALLEL_MASTER;
      break;
    case ST_OMP_PARALLEL_SECTIONS:
      omp_end_st = ST_OMP_END_PARALLEL_SECTIONS;
      break;
    case ST_OMP_SCOPE:
      omp_end_st = ST_OMP_END_SCOPE;
      break;
    case ST_OMP_SECTIONS:
      omp_end_st = ST_OMP_END_SECTIONS;
      break;
    case ST_OMP_ORDERED:
      omp_end_st = ST_OMP_END_ORDERED;
      break;
    case ST_OMP_CRITICAL:
      omp_end_st = ST_OMP_END_CRITICAL;
      break;
    case ST_OMP_MASKED:
      omp_end_st = ST_OMP_END_MASKED;
      break;
    case ST_OMP_MASTER:
      omp_end_st = ST_OMP_END_MASTER;
      break;
    case ST_OMP_SINGLE:
      omp_end_st = ST_OMP_END_SINGLE;
      break;
    case ST_OMP_TARGET:
      omp_end_st = ST_OMP_END_TARGET;
      break;
    case ST_OMP_TARGET_DATA:
      omp_end_st = ST_OMP_END_TARGET_DATA;
      break;
    case ST_OMP_TARGET_PARALLEL:
      omp_end_st = ST_OMP_END_TARGET_PARALLEL;
      break;
    case ST_OMP_TARGET_TEAMS:
      omp_end_st = ST_OMP_END_TARGET_TEAMS;
      break;
    case ST_OMP_TASK:
      omp_end_st = ST_OMP_END_TASK;
      break;
    case ST_OMP_TASKGROUP:
      omp_end_st = ST_OMP_END_TASKGROUP;
      break;
    case ST_OMP_TEAMS:
      omp_end_st = ST_OMP_END_TEAMS;
      break;
    case ST_OMP_TEAMS_DISTRIBUTE:
      omp_end_st = ST_OMP_END_TEAMS_DISTRIBUTE;
      break;
    case ST_OMP_DISTRIBUTE:
      omp_end_st = ST_OMP_END_DISTRIBUTE;
      break;
    case ST_OMP_WORKSHARE:
      omp_end_st = ST_OMP_END_WORKSHARE;
      break;
    case ST_OMP_PARALLEL_WORKSHARE:
      omp_end_st = ST_OMP_END_PARALLEL_WORKSHARE;
      break;
    default:
      gcc_unreachable ();
    }

  bool block_construct = false;
  gfc_namespace *my_ns = NULL;
  gfc_namespace *my_parent = NULL;

  first_st = st = next_statement ();

  if (st == ST_BLOCK)
    {
      /* Adjust state to a strictly-structured block, now that we found that
	 the body starts with a BLOCK construct.  */
      s.state = COMP_OMP_STRICTLY_STRUCTURED_BLOCK;

      block_construct = true;
      gfc_notify_std (GFC_STD_F2008, "BLOCK construct at %C");

      my_ns = gfc_build_block_ns (gfc_current_ns);
      new_st.op = EXEC_BLOCK;
      new_st.ext.block.ns = my_ns;
      new_st.ext.block.assoc = NULL;
      accept_statement (ST_BLOCK);

      push_state (&s2, COMP_BLOCK, my_ns->proc_name);
      gfc_current_ns = my_ns;
      my_parent = my_ns->parent;
      if (omp_st == ST_OMP_SECTIONS
	  || omp_st == ST_OMP_PARALLEL_SECTIONS)
	{
	  np = new_level (cp);
	  np->op = cp->op;
	}

      first_st = next_statement ();
      st = parse_spec (first_st);
    }

  if (omp_end_st == ST_OMP_END_TARGET)
    switch (first_st)
      {
      case ST_OMP_TEAMS:
      case ST_OMP_TEAMS_DISTRIBUTE:
      case ST_OMP_TEAMS_DISTRIBUTE_SIMD:
      case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      case ST_OMP_TEAMS_LOOP:
	{
	  gfc_state_data *stk = gfc_state_stack->previous;
	  if (stk->state == COMP_OMP_STRICTLY_STRUCTURED_BLOCK)
	    stk = stk->previous;
	  stk->tail->ext.omp_clauses->target_first_st_is_teams = true;
	  break;
	}
      default:
	break;
      }

  do
    {
      if (workshare_stmts_only)
	{
	  /* Inside of !$omp workshare, only
	     scalar assignments
	     array assignments
	     where statements and constructs
	     forall statements and constructs
	     !$omp atomic
	     !$omp critical
	     !$omp parallel
	     are allowed.  For !$omp critical these
	     restrictions apply recursively.  */
	  bool cycle = true;

	  for (;;)
	    {
	      switch (st)
		{
		case ST_NONE:
		  unexpected_eof ();

		case ST_ASSIGNMENT:
		case ST_WHERE:
		case ST_FORALL:
		  accept_statement (st);
		  break;

		case ST_WHERE_BLOCK:
		  parse_where_block ();
		  break;

		case ST_FORALL_BLOCK:
		  parse_forall_block ();
		  break;

		case ST_OMP_ALLOCATE_EXEC:
		case ST_OMP_ALLOCATORS:
		  st = parse_openmp_allocate_block (st);
		  continue;

		case ST_OMP_ASSUME:
		case ST_OMP_PARALLEL:
		case ST_OMP_PARALLEL_MASKED:
		case ST_OMP_PARALLEL_MASTER:
		case ST_OMP_PARALLEL_SECTIONS:
		  st = parse_omp_structured_block (st, false);
		  continue;

		case ST_OMP_PARALLEL_WORKSHARE:
		case ST_OMP_CRITICAL:
		  st = parse_omp_structured_block (st, true);
		  continue;

		case ST_OMP_PARALLEL_DO:
		case ST_OMP_PARALLEL_DO_SIMD:
		  st = parse_omp_do (st);
		  continue;

		case ST_OMP_ATOMIC:
		  st = parse_omp_oacc_atomic (true);
		  continue;

		default:
		  cycle = false;
		  break;
		}

	      if (!cycle)
		break;

	      st = next_statement ();
	    }
	}
      else
	st = parse_executable (st);
      if (st == ST_NONE)
	unexpected_eof ();
      else if (st == ST_OMP_SECTION
	       && (omp_st == ST_OMP_SECTIONS
		   || omp_st == ST_OMP_PARALLEL_SECTIONS))
	{
	  np = new_level (np);
	  np->op = cp->op;
	  np->block = NULL;
	  st = next_statement ();
	}
      else if (block_construct && st == ST_END_BLOCK)
	{
	  accept_statement (st);
	  gfc_current_ns->code = gfc_state_stack->head;
	  gfc_current_ns = my_parent;
	  pop_state ();  /* Inner BLOCK */
	  pop_state ();  /* Outer COMP_OMP_STRICTLY_STRUCTURED_BLOCK */

	  st = next_statement ();
	  if (st == omp_end_st)
	    {
	      accept_statement (st);
	      st = next_statement ();
	    }
	  return st;
	}
      else if (st != omp_end_st || block_construct)
	{
	  unexpected_statement (st);
	  st = next_statement ();
	}
    }
  while (st != omp_end_st);

  switch (new_st.op)
    {
    case EXEC_OMP_END_NOWAIT:
      if (cp->ext.omp_clauses->nowait && new_st.ext.omp_bool)
	gfc_error_now ("Duplicated NOWAIT clause on %s and %s at %C",
		       gfc_ascii_statement (omp_st),
		       gfc_ascii_statement (omp_end_st));
      cp->ext.omp_clauses->nowait |= new_st.ext.omp_bool;
      break;
    case EXEC_OMP_END_CRITICAL:
      if (((cp->ext.omp_clauses->critical_name == NULL)
	    ^ (new_st.ext.omp_name == NULL))
	  || (new_st.ext.omp_name != NULL
	      && strcmp (cp->ext.omp_clauses->critical_name,
			 new_st.ext.omp_name) != 0))
	gfc_error ("Name after !$omp critical and !$omp end critical does "
		   "not match at %C");
      free (CONST_CAST (char *, new_st.ext.omp_name));
      new_st.ext.omp_name = NULL;
      break;
    case EXEC_OMP_END_SINGLE:
      if (cp->ext.omp_clauses->nowait && new_st.ext.omp_clauses->nowait)
	gfc_error_now ("Duplicated NOWAIT clause on %s and %s at %C",
		       gfc_ascii_statement (omp_st),
		       gfc_ascii_statement (omp_end_st));
      cp->ext.omp_clauses->nowait |= new_st.ext.omp_clauses->nowait;
      if (cp->ext.omp_clauses->lists[OMP_LIST_COPYPRIVATE])
	{
	  gfc_omp_namelist *nl;
	  for (nl = cp->ext.omp_clauses->lists[OMP_LIST_COPYPRIVATE];
	      nl->next; nl = nl->next)
	    ;
	  nl->next = new_st.ext.omp_clauses->lists[OMP_LIST_COPYPRIVATE];
	}
      else
	cp->ext.omp_clauses->lists[OMP_LIST_COPYPRIVATE]
	  = new_st.ext.omp_clauses->lists[OMP_LIST_COPYPRIVATE];
      new_st.ext.omp_clauses->lists[OMP_LIST_COPYPRIVATE] = NULL;
      gfc_free_omp_clauses (new_st.ext.omp_clauses);
      break;
    case EXEC_NOP:
      break;
    default:
      gcc_unreachable ();
    }

  gfc_clear_new_st ();
  gfc_commit_symbols ();
  gfc_warning_check ();
  pop_state ();
  st = next_statement ();
  return st;
}


/* Accept a series of executable statements.  We return the first
   statement that doesn't fit to the caller.  Any block statements are
   passed on to the correct handler, which usually passes the buck
   right back here.  */

static gfc_statement
parse_executable (gfc_statement st)
{
  int close_flag;
  in_exec_part = true;

  if (st == ST_NONE)
    st = next_statement ();

  for (;;)
    {
      close_flag = check_do_closure ();
      if (close_flag)
	switch (st)
	  {
	  case ST_GOTO:
	  case ST_END_PROGRAM:
	  case ST_RETURN:
	  case ST_EXIT:
	  case ST_END_FUNCTION:
	  case ST_CYCLE:
	  case ST_PAUSE:
	  case ST_STOP:
	  case ST_ERROR_STOP:
	  case ST_END_SUBROUTINE:

	  case ST_DO:
	  case ST_FORALL:
	  case ST_WHERE:
	  case ST_SELECT_CASE:
	    gfc_error ("%s statement at %C cannot terminate a non-block "
		       "DO loop", gfc_ascii_statement (st));
	    break;

	  default:
	    break;
	  }

      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_DATA:
	  gfc_notify_std (GFC_STD_F95_OBS, "DATA statement at %C after the "
			  "first executable statement");
	  /* Fall through.  */

	case ST_FORMAT:
	case ST_ENTRY:
	case_executable:
	  accept_statement (st);
	  if (close_flag == 1)
	    return ST_IMPLIED_ENDDO;
	  break;

	case ST_BLOCK:
	  parse_block_construct ();
	  break;

	case ST_ASSOCIATE:
	  parse_associate ();
	  break;

	case ST_IF_BLOCK:
	  parse_if_block ();
	  break;

	case ST_SELECT_CASE:
	  parse_select_block ();
	  break;

	case ST_SELECT_TYPE:
	  parse_select_type_block ();
	  break;

	case ST_SELECT_RANK:
	  parse_select_rank_block ();
	  break;

	case ST_DO:
	  parse_do_block ();
	  if (check_do_closure () == 1)
	    return ST_IMPLIED_ENDDO;
	  break;

	case ST_CRITICAL:
	  parse_critical_block ();
	  break;

	case ST_WHERE_BLOCK:
	  parse_where_block ();
	  break;

	case ST_FORALL_BLOCK:
	  parse_forall_block ();
	  break;

	case ST_OACC_PARALLEL_LOOP:
	case ST_OACC_KERNELS_LOOP:
	case ST_OACC_SERIAL_LOOP:
	case ST_OACC_LOOP:
	  st = parse_oacc_loop (st);
	  if (st == ST_IMPLIED_ENDDO)
	    return st;
	  continue;

	case ST_OACC_PARALLEL:
	case ST_OACC_KERNELS:
	case ST_OACC_SERIAL:
	case ST_OACC_DATA:
	case ST_OACC_HOST_DATA:
	  parse_oacc_structured_block (st);
	  break;

	case ST_OMP_ALLOCATE_EXEC:
	case ST_OMP_ALLOCATORS:
	  st = parse_openmp_allocate_block (st);
	  continue;

	case ST_OMP_ASSUME:
	case ST_OMP_PARALLEL:
	case ST_OMP_PARALLEL_MASKED:
	case ST_OMP_PARALLEL_MASTER:
	case ST_OMP_PARALLEL_SECTIONS:
	case ST_OMP_ORDERED:
	case ST_OMP_CRITICAL:
	case ST_OMP_MASKED:
	case ST_OMP_MASTER:
	case ST_OMP_SCOPE:
	case ST_OMP_SECTIONS:
	case ST_OMP_SINGLE:
	case ST_OMP_TARGET:
	case ST_OMP_TARGET_DATA:
	case ST_OMP_TARGET_PARALLEL:
	case ST_OMP_TARGET_TEAMS:
	case ST_OMP_TEAMS:
	case ST_OMP_TASK:
	case ST_OMP_TASKGROUP:
	  st = parse_omp_structured_block (st, false);
	  continue;

	case ST_OMP_WORKSHARE:
	case ST_OMP_PARALLEL_WORKSHARE:
	  st = parse_omp_structured_block (st, true);
	  continue;

	case ST_OMP_DISTRIBUTE:
	case ST_OMP_DISTRIBUTE_PARALLEL_DO:
	case ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
	case ST_OMP_DISTRIBUTE_SIMD:
	case ST_OMP_DO:
	case ST_OMP_DO_SIMD:
	case ST_OMP_LOOP:
	case ST_OMP_PARALLEL_DO:
	case ST_OMP_PARALLEL_DO_SIMD:
	case ST_OMP_PARALLEL_LOOP:
	case ST_OMP_PARALLEL_MASKED_TASKLOOP:
	case ST_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
	case ST_OMP_PARALLEL_MASTER_TASKLOOP:
	case ST_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
	case ST_OMP_MASKED_TASKLOOP:
	case ST_OMP_MASKED_TASKLOOP_SIMD:
	case ST_OMP_MASTER_TASKLOOP:
	case ST_OMP_MASTER_TASKLOOP_SIMD:
	case ST_OMP_SIMD:
	case ST_OMP_TARGET_PARALLEL_DO:
	case ST_OMP_TARGET_PARALLEL_DO_SIMD:
	case ST_OMP_TARGET_PARALLEL_LOOP:
	case ST_OMP_TARGET_SIMD:
	case ST_OMP_TARGET_TEAMS_DISTRIBUTE:
	case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	case ST_OMP_TARGET_TEAMS_LOOP:
	case ST_OMP_TASKLOOP:
	case ST_OMP_TASKLOOP_SIMD:
	case ST_OMP_TEAMS_DISTRIBUTE:
	case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case ST_OMP_TEAMS_DISTRIBUTE_SIMD:
	case ST_OMP_TEAMS_LOOP:
	  st = parse_omp_do (st);
	  if (st == ST_IMPLIED_ENDDO)
	    return st;
	  continue;

	case ST_OACC_ATOMIC:
	  st = parse_omp_oacc_atomic (false);
	  continue;

	case ST_OMP_ATOMIC:
	  st = parse_omp_oacc_atomic (true);
	  continue;

	default:
	  return st;
	}

      if (directive_unroll != -1)
	gfc_error ("%<GCC unroll%> directive not at the start of a loop at %C");

      if (directive_ivdep)
	gfc_error ("%<GCC ivdep%> directive not at the start of a loop at %C");

      if (directive_vector)
	gfc_error ("%<GCC vector%> directive not at the start of a loop at %C");

      if (directive_novector)
	gfc_error ("%<GCC novector%> "
		   "directive not at the start of a loop at %C");

      st = next_statement ();
    }
}


/* Fix the symbols for sibling functions.  These are incorrectly added to
   the child namespace as the parser didn't know about this procedure.  */

static void
gfc_fixup_sibling_symbols (gfc_symbol *sym, gfc_namespace *siblings)
{
  gfc_namespace *ns;
  gfc_symtree *st;
  gfc_symbol *old_sym;

  for (ns = siblings; ns; ns = ns->sibling)
    {
      st = gfc_find_symtree (ns->sym_root, sym->name);

      if (!st || (st->n.sym->attr.dummy && ns == st->n.sym->ns))
	goto fixup_contained;

      if ((st->n.sym->attr.flavor == FL_DERIVED
	   && sym->attr.generic && sym->attr.function)
	  ||(sym->attr.flavor == FL_DERIVED
	     && st->n.sym->attr.generic && st->n.sym->attr.function))
	goto fixup_contained;

      old_sym = st->n.sym;
      if (old_sym->ns == ns
	    && !old_sym->attr.contained

	    /* By 14.6.1.3, host association should be excluded
	       for the following.  */
	    && !(old_sym->attr.external
		  || (old_sym->ts.type != BT_UNKNOWN
			&& !old_sym->attr.implicit_type)
		  || old_sym->attr.flavor == FL_PARAMETER
		  || old_sym->attr.use_assoc
		  || old_sym->attr.in_common
		  || old_sym->attr.in_equivalence
		  || old_sym->attr.data
		  || old_sym->attr.dummy
		  || old_sym->attr.result
		  || old_sym->attr.dimension
		  || old_sym->attr.allocatable
		  || old_sym->attr.intrinsic
		  || old_sym->attr.generic
		  || old_sym->attr.flavor == FL_NAMELIST
		  || old_sym->attr.flavor == FL_LABEL
		  || old_sym->attr.proc == PROC_ST_FUNCTION))
	{
	  /* Replace it with the symbol from the parent namespace.  */
	  st->n.sym = sym;
	  sym->refs++;

	  gfc_release_symbol (old_sym);
	}

fixup_contained:
      /* Do the same for any contained procedures.  */
      gfc_fixup_sibling_symbols (sym, ns->contained);
    }
}

static void
parse_contained (int module)
{
  gfc_namespace *ns, *parent_ns, *tmp;
  gfc_state_data s1, s2;
  gfc_statement st;
  gfc_symbol *sym;
  gfc_entry_list *el;
  locus old_loc;
  int contains_statements = 0;
  int seen_error = 0;

  push_state (&s1, COMP_CONTAINS, NULL);
  parent_ns = gfc_current_ns;

  do
    {
      gfc_current_ns = gfc_get_namespace (parent_ns, 1);

      gfc_current_ns->sibling = parent_ns->contained;
      parent_ns->contained = gfc_current_ns;

 next:
      /* Process the next available statement.  We come here if we got an error
	 and rejected the last statement.  */
      old_loc = gfc_current_locus;
      st = next_statement ();

      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_FUNCTION:
	case ST_SUBROUTINE:
	  contains_statements = 1;
	  accept_statement (st);

	  push_state (&s2,
		      (st == ST_FUNCTION) ? COMP_FUNCTION : COMP_SUBROUTINE,
		      gfc_new_block);

	  /* For internal procedures, create/update the symbol in the
	     parent namespace.  */

	  if (!module)
	    {
	      if (gfc_get_symbol (gfc_new_block->name, parent_ns, &sym))
		gfc_error ("Contained procedure %qs at %C is already "
			   "ambiguous", gfc_new_block->name);
	      else
		{
		  if (gfc_add_procedure (&sym->attr, PROC_INTERNAL,
					 sym->name,
					 &gfc_new_block->declared_at))
		    {
		      if (st == ST_FUNCTION)
			gfc_add_function (&sym->attr, sym->name,
					  &gfc_new_block->declared_at);
		      else
			gfc_add_subroutine (&sym->attr, sym->name,
					    &gfc_new_block->declared_at);
		    }
		}

	      gfc_commit_symbols ();
	    }
	  else
	    sym = gfc_new_block;

	  /* Mark this as a contained function, so it isn't replaced
	     by other module functions.  */
	  sym->attr.contained = 1;

	  /* Set implicit_pure so that it can be reset if any of the
	     tests for purity fail.  This is used for some optimisation
	     during translation.  */
	  if (!sym->attr.pure)
	    sym->attr.implicit_pure = 1;

	  parse_progunit (ST_NONE);

	  /* Fix up any sibling functions that refer to this one.  */
	  gfc_fixup_sibling_symbols (sym, gfc_current_ns);
	  /* Or refer to any of its alternate entry points.  */
	  for (el = gfc_current_ns->entries; el; el = el->next)
	    gfc_fixup_sibling_symbols (el->sym, gfc_current_ns);

	  gfc_current_ns->code = s2.head;
	  gfc_current_ns = parent_ns;

	  pop_state ();
	  break;

	/* These statements are associated with the end of the host unit.  */
	case ST_END_FUNCTION:
	case ST_END_MODULE:
	case ST_END_SUBMODULE:
	case ST_END_PROGRAM:
	case ST_END_SUBROUTINE:
	  accept_statement (st);
	  gfc_current_ns->code = s1.head;
	  break;

	default:
	  gfc_error ("Unexpected %s statement in CONTAINS section at %C",
		     gfc_ascii_statement (st));
	  reject_statement ();
	  seen_error = 1;
	  goto next;
	  break;
	}
    }
  while (st != ST_END_FUNCTION && st != ST_END_SUBROUTINE
	 && st != ST_END_MODULE && st != ST_END_SUBMODULE
	 && st != ST_END_PROGRAM);

  /* The first namespace in the list is guaranteed to not have
     anything (worthwhile) in it.  */
  tmp = gfc_current_ns;
  gfc_current_ns = parent_ns;
  if (seen_error && tmp->refs > 1)
    gfc_free_namespace (tmp);

  ns = gfc_current_ns->contained;
  gfc_current_ns->contained = ns->sibling;
  gfc_free_namespace (ns);

  pop_state ();
  if (!contains_statements)
    gfc_notify_std (GFC_STD_F2008, "CONTAINS statement without "
		    "FUNCTION or SUBROUTINE statement at %L", &old_loc);
}


/* The result variable in a MODULE PROCEDURE needs to be created and
    its characteristics copied from the interface since it is neither
    declared in the procedure declaration nor in the specification
    part.  */

static void
get_modproc_result (void)
{
  gfc_symbol *proc;
  if (gfc_state_stack->previous
      && gfc_state_stack->previous->state == COMP_CONTAINS
      && gfc_state_stack->previous->previous->state == COMP_SUBMODULE)
    {
      proc = gfc_current_ns->proc_name ? gfc_current_ns->proc_name : NULL;
      if (proc != NULL
	  && proc->attr.function
	  && proc->tlink
	  && proc->tlink->result
	  && proc->tlink->result != proc->tlink)
	{
	  gfc_copy_dummy_sym (&proc->result, proc->tlink->result, 1);
	  gfc_set_sym_referenced (proc->result);
	  proc->result->attr.if_source = IFSRC_DECL;
	  gfc_commit_symbol (proc->result);
	}
    }
}


/* Parse a PROGRAM, SUBROUTINE, FUNCTION unit or BLOCK construct.  */

static void
parse_progunit (gfc_statement st)
{
  gfc_state_data *p;
  int n;

  gfc_adjust_builtins ();

  if (gfc_new_block
      && gfc_new_block->abr_modproc_decl
      && gfc_new_block->attr.function)
    get_modproc_result ();

  st = parse_spec (st);
  switch (st)
    {
    case ST_NONE:
      unexpected_eof ();

    case ST_CONTAINS:
      /* This is not allowed within BLOCK!  */
      if (gfc_current_state () != COMP_BLOCK)
	goto contains;
      break;

    case_end:
      accept_statement (st);
      goto done;

    default:
      break;
    }

  if (gfc_current_state () == COMP_FUNCTION)
    gfc_check_function_type (gfc_current_ns);

loop:
  for (;;)
    {
      st = parse_executable (st);

      switch (st)
	{
	case ST_NONE:
	  unexpected_eof ();

	case ST_CONTAINS:
	  /* This is not allowed within BLOCK!  */
	  if (gfc_current_state () != COMP_BLOCK)
	    goto contains;
	  break;

	case_end:
	  accept_statement (st);
	  goto done;

	default:
	  break;
	}

      unexpected_statement (st);
      reject_statement ();
      st = next_statement ();
    }

contains:
  n = 0;

  for (p = gfc_state_stack; p; p = p->previous)
    if (p->state == COMP_CONTAINS)
      n++;

  if (gfc_find_state (COMP_MODULE) == true
      || gfc_find_state (COMP_SUBMODULE) == true)
    n--;

  if (n > 0)
    {
      gfc_error ("CONTAINS statement at %C is already in a contained "
		 "program unit");
      reject_statement ();
      st = next_statement ();
      goto loop;
    }

  parse_contained (0);

done:
  gfc_current_ns->code = gfc_state_stack->head;
}


/* Come here to complain about a global symbol already in use as
   something else.  */

void
gfc_global_used (gfc_gsymbol *sym, locus *where)
{
  const char *name;

  if (where == NULL)
    where = &gfc_current_locus;

  switch(sym->type)
    {
    case GSYM_PROGRAM:
      name = "PROGRAM";
      break;
    case GSYM_FUNCTION:
      name = "FUNCTION";
      break;
    case GSYM_SUBROUTINE:
      name = "SUBROUTINE";
      break;
    case GSYM_COMMON:
      name = "COMMON";
      break;
    case GSYM_BLOCK_DATA:
      name = "BLOCK DATA";
      break;
    case GSYM_MODULE:
      name = "MODULE";
      break;
    default:
      name = NULL;
    }

  if (name)
    {
      if (sym->binding_label)
	gfc_error ("Global binding name %qs at %L is already being used "
		   "as a %s at %L", sym->binding_label, where, name,
		   &sym->where);
      else
	gfc_error ("Global name %qs at %L is already being used as "
		   "a %s at %L", sym->name, where, name, &sym->where);
    }
  else
    {
      if (sym->binding_label)
	gfc_error ("Global binding name %qs at %L is already being used "
		   "at %L", sym->binding_label, where, &sym->where);
      else
	gfc_error ("Global name %qs at %L is already being used at %L",
		   sym->name, where, &sym->where);
    }
}


/* Parse a block data program unit.  */

static void
parse_block_data (void)
{
  gfc_statement st;
  static locus blank_locus;
  static int blank_block=0;
  gfc_gsymbol *s;

  gfc_current_ns->proc_name = gfc_new_block;
  gfc_current_ns->is_block_data = 1;

  if (gfc_new_block == NULL)
    {
      if (blank_block)
       gfc_error ("Blank BLOCK DATA at %C conflicts with "
		  "prior BLOCK DATA at %L", &blank_locus);
      else
       {
	 blank_block = 1;
	 blank_locus = gfc_current_locus;
       }
    }
  else
    {
      s = gfc_get_gsymbol (gfc_new_block->name, false);
      if (s->defined
	  || (s->type != GSYM_UNKNOWN && s->type != GSYM_BLOCK_DATA))
       gfc_global_used (s, &gfc_new_block->declared_at);
      else
       {
	 s->type = GSYM_BLOCK_DATA;
	 s->where = gfc_new_block->declared_at;
	 s->defined = 1;
       }
    }

  st = parse_spec (ST_NONE);

  while (st != ST_END_BLOCK_DATA)
    {
      gfc_error ("Unexpected %s statement in BLOCK DATA at %C",
		 gfc_ascii_statement (st));
      reject_statement ();
      st = next_statement ();
    }
}


/* Following the association of the ancestor (sub)module symbols, they
   must be set host rather than use associated and all must be public.
   They are flagged up by 'used_in_submodule' so that they can be set
   DECL_EXTERNAL in trans_decl.c(gfc_finish_var_decl).  Otherwise the
   linker chokes on multiple symbol definitions.  */

static void
set_syms_host_assoc (gfc_symbol *sym)
{
  gfc_component *c;
  const char dot[2] = ".";
  /* Symbols take the form module.submodule_ or module.name_. */
  char parent1[2 * GFC_MAX_SYMBOL_LEN + 2];
  char parent2[2 * GFC_MAX_SYMBOL_LEN + 2];

  if (sym == NULL)
    return;

  if (sym->attr.module_procedure)
    sym->attr.external = 0;

  sym->attr.use_assoc = 0;
  sym->attr.host_assoc = 1;
  sym->attr.used_in_submodule =1;

  if (sym->attr.flavor == FL_DERIVED)
    {
      /* Derived types with PRIVATE components that are declared in
	 modules other than the parent module must not be changed to be
	 PUBLIC. The 'use-assoc' attribute must be reset so that the
	 test in symbol.cc(gfc_find_component) works correctly. This is
	 not necessary for PRIVATE symbols since they are not read from
	 the module.  */
      memset(parent1, '\0', sizeof(parent1));
      memset(parent2, '\0', sizeof(parent2));
      strcpy (parent1, gfc_new_block->name);
      strcpy (parent2, sym->module);
      if (strcmp (strtok (parent1, dot), strtok (parent2, dot)) == 0)
	{
	  for (c = sym->components; c; c = c->next)
	    c->attr.access = ACCESS_PUBLIC;
	}
      else
	{
	  sym->attr.use_assoc = 1;
	  sym->attr.host_assoc = 0;
	}
    }
}

/* Parse a module subprogram.  */

static void
parse_module (void)
{
  gfc_statement st;
  gfc_gsymbol *s;

  s = gfc_get_gsymbol (gfc_new_block->name, false);
  if (s->defined || (s->type != GSYM_UNKNOWN && s->type != GSYM_MODULE))
    gfc_global_used (s, &gfc_new_block->declared_at);
  else
    {
      s->type = GSYM_MODULE;
      s->where = gfc_new_block->declared_at;
      s->defined = 1;
    }

  /* Something is nulling the module_list after this point. This is good
     since it allows us to 'USE' the parent modules that the submodule
     inherits and to set (most) of the symbols as host associated.  */
  if (gfc_current_state () == COMP_SUBMODULE)
    {
      use_modules ();
      gfc_traverse_ns (gfc_current_ns, set_syms_host_assoc);
    }

  st = parse_spec (ST_NONE);

loop:
  switch (st)
    {
    case ST_NONE:
      unexpected_eof ();

    case ST_CONTAINS:
      parse_contained (1);
      break;

    case ST_END_MODULE:
    case ST_END_SUBMODULE:
      accept_statement (st);
      break;

    default:
      gfc_error ("Unexpected %s statement in MODULE at %C",
		 gfc_ascii_statement (st));
      reject_statement ();
      st = next_statement ();
      goto loop;
    }
  s->ns = gfc_current_ns;
}


/* Add a procedure name to the global symbol table.  */

static void
add_global_procedure (bool sub)
{
  gfc_gsymbol *s;

  /* Only in Fortran 2003: For procedures with a binding label also the Fortran
     name is a global identifier.  */
  if (!gfc_new_block->binding_label || gfc_notification_std (GFC_STD_F2008))
    {
      s = gfc_get_gsymbol (gfc_new_block->name, false);

      if (s->defined
	  || (s->type != GSYM_UNKNOWN
	      && s->type != (sub ? GSYM_SUBROUTINE : GSYM_FUNCTION)))
	{
	  gfc_global_used (s, &gfc_new_block->declared_at);
	  /* Silence follow-up errors.  */
	  gfc_new_block->binding_label = NULL;
	}
      else
	{
	  s->type = sub ? GSYM_SUBROUTINE : GSYM_FUNCTION;
	  s->sym_name = gfc_new_block->name;
	  s->where = gfc_new_block->declared_at;
	  s->defined = 1;
	  s->ns = gfc_current_ns;
	}
    }

  /* Don't add the symbol multiple times.  */
  if (gfc_new_block->binding_label
      && (!gfc_notification_std (GFC_STD_F2008)
          || strcmp (gfc_new_block->name, gfc_new_block->binding_label) != 0))
    {
      s = gfc_get_gsymbol (gfc_new_block->binding_label, true);

      if (s->defined
	  || (s->type != GSYM_UNKNOWN
	      && s->type != (sub ? GSYM_SUBROUTINE : GSYM_FUNCTION)))
	{
	  gfc_global_used (s, &gfc_new_block->declared_at);
	  /* Silence follow-up errors.  */
	  gfc_new_block->binding_label = NULL;
	}
      else
	{
	  s->type = sub ? GSYM_SUBROUTINE : GSYM_FUNCTION;
	  s->sym_name = gfc_new_block->name;
	  s->binding_label = gfc_new_block->binding_label;
	  s->where = gfc_new_block->declared_at;
	  s->defined = 1;
	  s->ns = gfc_current_ns;
	}
    }
}


/* Add a program to the global symbol table.  */

static void
add_global_program (void)
{
  gfc_gsymbol *s;

  if (gfc_new_block == NULL)
    return;
  s = gfc_get_gsymbol (gfc_new_block->name, false);

  if (s->defined || (s->type != GSYM_UNKNOWN && s->type != GSYM_PROGRAM))
    gfc_global_used (s, &gfc_new_block->declared_at);
  else
    {
      s->type = GSYM_PROGRAM;
      s->where = gfc_new_block->declared_at;
      s->defined = 1;
      s->ns = gfc_current_ns;
    }
}


/* Resolve all the program units.  */
static void
resolve_all_program_units (gfc_namespace *gfc_global_ns_list)
{
  gfc_derived_types = NULL;
  gfc_current_ns = gfc_global_ns_list;
  for (; gfc_current_ns; gfc_current_ns = gfc_current_ns->sibling)
    {
      if (gfc_current_ns->proc_name
	  && gfc_current_ns->proc_name->attr.flavor == FL_MODULE)
	continue; /* Already resolved.  */

      if (gfc_current_ns->proc_name)
	gfc_current_locus = gfc_current_ns->proc_name->declared_at;
      gfc_resolve (gfc_current_ns);
      gfc_current_ns->derived_types = gfc_derived_types;
      gfc_derived_types = NULL;
    }
}


static void
clean_up_modules (gfc_gsymbol *&gsym)
{
  if (gsym == NULL)
    return;

  clean_up_modules (gsym->left);
  clean_up_modules (gsym->right);

  if (gsym->type != GSYM_MODULE)
    return;

  if (gsym->ns)
    {
      gfc_current_ns = gsym->ns;
      gfc_derived_types = gfc_current_ns->derived_types;
      gfc_done_2 ();
      gsym->ns = NULL;
    }
  free (gsym);
  gsym = NULL;
}


/* Translate all the program units. This could be in a different order
   to resolution if there are forward references in the file.  */
static void
translate_all_program_units (gfc_namespace *gfc_global_ns_list)
{
  int errors;

  gfc_current_ns = gfc_global_ns_list;
  gfc_get_errors (NULL, &errors);

  /* We first translate all modules to make sure that later parts
     of the program can use the decl. Then we translate the nonmodules.  */

  for (; !errors && gfc_current_ns; gfc_current_ns = gfc_current_ns->sibling)
    {
      if (!gfc_current_ns->proc_name
	  || gfc_current_ns->proc_name->attr.flavor != FL_MODULE)
	continue;

      gfc_current_locus = gfc_current_ns->proc_name->declared_at;
      gfc_derived_types = gfc_current_ns->derived_types;
      gfc_generate_module_code (gfc_current_ns);
      gfc_current_ns->translated = 1;
    }

  gfc_current_ns = gfc_global_ns_list;
  for (; !errors && gfc_current_ns; gfc_current_ns = gfc_current_ns->sibling)
    {
      if (gfc_current_ns->proc_name
	  && gfc_current_ns->proc_name->attr.flavor == FL_MODULE)
	continue;

      gfc_current_locus = gfc_current_ns->proc_name->declared_at;
      gfc_derived_types = gfc_current_ns->derived_types;
      gfc_generate_code (gfc_current_ns);
      gfc_current_ns->translated = 1;
    }

  /* Clean up all the namespaces after translation.  */
  gfc_current_ns = gfc_global_ns_list;
  for (;gfc_current_ns;)
    {
      gfc_namespace *ns;

      if (gfc_current_ns->proc_name
	  && gfc_current_ns->proc_name->attr.flavor == FL_MODULE)
	{
	  gfc_current_ns = gfc_current_ns->sibling;
	  continue;
	}

      ns = gfc_current_ns->sibling;
      gfc_derived_types = gfc_current_ns->derived_types;
      gfc_done_2 ();
      gfc_current_ns = ns;
    }

  clean_up_modules (gfc_gsym_root);
}


/* Top level parser.  */

bool
gfc_parse_file (void)
{
  int seen_program, errors_before, errors;
  gfc_state_data top, s;
  gfc_statement st;
  locus prog_locus;
  gfc_namespace *next;

  gfc_start_source_files ();

  top.state = COMP_NONE;
  top.sym = NULL;
  top.previous = NULL;
  top.head = top.tail = NULL;
  top.do_variable = NULL;

  gfc_state_stack = &top;

  gfc_clear_new_st ();

  gfc_statement_label = NULL;

  if (setjmp (eof_buf))
    return false;	/* Come here on unexpected EOF */

  /* Prepare the global namespace that will contain the
     program units.  */
  gfc_global_ns_list = next = NULL;

  seen_program = 0;
  errors_before = 0;

  /* Exit early for empty files.  */
  if (gfc_at_eof ())
    goto done;

  in_specification_block = true;
loop:
  gfc_init_2 ();
  st = next_statement ();
  switch (st)
    {
    case ST_NONE:
      gfc_done_2 ();
      goto done;

    case ST_PROGRAM:
      if (seen_program)
	goto duplicate_main;
      seen_program = 1;
      prog_locus = gfc_current_locus;

      push_state (&s, COMP_PROGRAM, gfc_new_block);
      main_program_symbol (gfc_current_ns, gfc_new_block->name);
      accept_statement (st);
      add_global_program ();
      parse_progunit (ST_NONE);
      goto prog_units;

    case ST_SUBROUTINE:
      add_global_procedure (true);
      push_state (&s, COMP_SUBROUTINE, gfc_new_block);
      accept_statement (st);
      parse_progunit (ST_NONE);
      goto prog_units;

    case ST_FUNCTION:
      add_global_procedure (false);
      push_state (&s, COMP_FUNCTION, gfc_new_block);
      accept_statement (st);
      parse_progunit (ST_NONE);
      goto prog_units;

    case ST_BLOCK_DATA:
      push_state (&s, COMP_BLOCK_DATA, gfc_new_block);
      accept_statement (st);
      parse_block_data ();
      break;

    case ST_MODULE:
      push_state (&s, COMP_MODULE, gfc_new_block);
      accept_statement (st);

      gfc_get_errors (NULL, &errors_before);
      parse_module ();
      break;

    case ST_SUBMODULE:
      push_state (&s, COMP_SUBMODULE, gfc_new_block);
      accept_statement (st);

      gfc_get_errors (NULL, &errors_before);
      parse_module ();
      break;

    /* Anything else starts a nameless main program block.  */
    default:
      if (seen_program)
	goto duplicate_main;
      seen_program = 1;
      prog_locus = gfc_current_locus;

      push_state (&s, COMP_PROGRAM, gfc_new_block);
      main_program_symbol (gfc_current_ns, "MAIN__");
      parse_progunit (st);
      goto prog_units;
    }

  /* Handle the non-program units.  */
  gfc_current_ns->code = s.head;

  gfc_resolve (gfc_current_ns);

  /* Fix the implicit_pure attribute for those procedures who should
     not have it.  */
  while (gfc_fix_implicit_pure (gfc_current_ns))
    ;

  /* Dump the parse tree if requested.  */
  if (flag_dump_fortran_original)
    gfc_dump_parse_tree (gfc_current_ns, stdout);

  gfc_get_errors (NULL, &errors);
  if (s.state == COMP_MODULE || s.state == COMP_SUBMODULE)
    {
      gfc_dump_module (s.sym->name, errors_before == errors);
      gfc_current_ns->derived_types = gfc_derived_types;
      gfc_derived_types = NULL;
      goto prog_units;
    }
  else
    {
      if (errors == 0)
	gfc_generate_code (gfc_current_ns);
      pop_state ();
      gfc_done_2 ();
    }

  goto loop;

prog_units:
  /* The main program and non-contained procedures are put
     in the global namespace list, so that they can be processed
     later and all their interfaces resolved.  */
  gfc_current_ns->code = s.head;
  if (next)
    {
      for (; next->sibling; next = next->sibling)
	;
      next->sibling = gfc_current_ns;
    }
  else
    gfc_global_ns_list = gfc_current_ns;

  next = gfc_current_ns;

  pop_state ();
  goto loop;

done:
  /* Do the resolution.  */
  resolve_all_program_units (gfc_global_ns_list);

  /* Go through all top-level namespaces and unset the implicit_pure
     attribute for any procedures that call something not pure or
     implicit_pure.  Because the a procedure marked as not implicit_pure
     in one sweep may be called by another routine, we repeat this
     process until there are no more changes.  */
  bool changed;
  do
    {
      changed = false;
      for (gfc_current_ns = gfc_global_ns_list; gfc_current_ns;
	   gfc_current_ns = gfc_current_ns->sibling)
	{
	  if (gfc_fix_implicit_pure (gfc_current_ns))
	    changed = true;
	}
    }
  while (changed);

  /* Fixup for external procedures and resolve 'omp requires'.  */
  int omp_requires;
  bool omp_target_seen;
  omp_requires = 0;
  omp_target_seen = false;
  for (gfc_current_ns = gfc_global_ns_list; gfc_current_ns;
       gfc_current_ns = gfc_current_ns->sibling)
    {
      omp_requires |= gfc_current_ns->omp_requires;
      omp_target_seen |= gfc_current_ns->omp_target_seen;
      gfc_check_externals (gfc_current_ns);
    }
  for (gfc_current_ns = gfc_global_ns_list; gfc_current_ns;
       gfc_current_ns = gfc_current_ns->sibling)
    gfc_check_omp_requires (gfc_current_ns, omp_requires);

  /* Populate omp_requires_mask (needed for resolving OpenMP
     metadirectives and declare variant).  */
  switch (omp_requires & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
    {
    case OMP_REQ_ATOMIC_MEM_ORDER_SEQ_CST:
      omp_requires_mask
	= (enum omp_requires) (omp_requires_mask | OMP_MEMORY_ORDER_SEQ_CST);
      break;
    case OMP_REQ_ATOMIC_MEM_ORDER_ACQ_REL:
      omp_requires_mask
	= (enum omp_requires) (omp_requires_mask | OMP_MEMORY_ORDER_ACQ_REL);
      break;
    case OMP_REQ_ATOMIC_MEM_ORDER_ACQUIRE:
      omp_requires_mask
	= (enum omp_requires) (omp_requires_mask | OMP_MEMORY_ORDER_ACQUIRE);
      break;
    case OMP_REQ_ATOMIC_MEM_ORDER_RELAXED:
      omp_requires_mask
	= (enum omp_requires) (omp_requires_mask | OMP_MEMORY_ORDER_RELAXED);
      break;
    case OMP_REQ_ATOMIC_MEM_ORDER_RELEASE:
      omp_requires_mask
	= (enum omp_requires) (omp_requires_mask | OMP_MEMORY_ORDER_RELEASE);
      break;
    }

  if (omp_target_seen)
    omp_requires_mask = (enum omp_requires) (omp_requires_mask
					     | OMP_REQUIRES_TARGET_USED);
  if (omp_requires & OMP_REQ_REVERSE_OFFLOAD)
    omp_requires_mask = (enum omp_requires) (omp_requires_mask
					     | OMP_REQUIRES_REVERSE_OFFLOAD);
  if (omp_requires & OMP_REQ_UNIFIED_ADDRESS)
    omp_requires_mask = (enum omp_requires) (omp_requires_mask
					     | OMP_REQUIRES_UNIFIED_ADDRESS);
  if (omp_requires & OMP_REQ_UNIFIED_SHARED_MEMORY)
    omp_requires_mask
	  = (enum omp_requires) (omp_requires_mask
				 | OMP_REQUIRES_UNIFIED_SHARED_MEMORY);
  if (omp_requires & OMP_REQ_DYNAMIC_ALLOCATORS)
    omp_requires_mask = (enum omp_requires) (omp_requires_mask
					     | OMP_REQUIRES_DYNAMIC_ALLOCATORS);
  /* Do the parse tree dump.  */
  gfc_current_ns = flag_dump_fortran_original ? gfc_global_ns_list : NULL;

  for (; gfc_current_ns; gfc_current_ns = gfc_current_ns->sibling)
    if (!gfc_current_ns->proc_name
	|| gfc_current_ns->proc_name->attr.flavor != FL_MODULE)
      {
	gfc_dump_parse_tree (gfc_current_ns, stdout);
	fputs ("------------------------------------------\n\n", stdout);
      }

  /* Dump C prototypes.  */
  if (flag_c_prototypes || flag_c_prototypes_external)
    {
      fprintf (stdout,
	       "#include <stddef.h>\n"
	       "#ifdef __cplusplus\n"
	       "#include <complex>\n"
	       "#define __GFORTRAN_FLOAT_COMPLEX std::complex<float>\n"
	       "#define __GFORTRAN_DOUBLE_COMPLEX std::complex<double>\n"
	       "#define __GFORTRAN_LONG_DOUBLE_COMPLEX std::complex<long double>\n"
	       "extern \"C\" {\n"
	       "#else\n"
	       "#define __GFORTRAN_FLOAT_COMPLEX float _Complex\n"
	       "#define __GFORTRAN_DOUBLE_COMPLEX double _Complex\n"
	       "#define __GFORTRAN_LONG_DOUBLE_COMPLEX long double _Complex\n"
	       "#endif\n\n");
    }

  /* First dump BIND(C) prototypes.  */
  if (flag_c_prototypes)
    {
      for (gfc_current_ns = gfc_global_ns_list; gfc_current_ns;
	   gfc_current_ns = gfc_current_ns->sibling)
	gfc_dump_c_prototypes (gfc_current_ns, stdout);
    }

  /* Dump external prototypes.  */
  if (flag_c_prototypes_external)
    gfc_dump_external_c_prototypes (stdout);

  if (flag_c_prototypes || flag_c_prototypes_external)
    fprintf (stdout, "\n#ifdef __cplusplus\n}\n#endif\n");

  /* Do the translation.  */
  translate_all_program_units (gfc_global_ns_list);

  /* Dump the global symbol ist.  We only do this here because part
     of it is generated after mangling the identifiers in
     trans-decl.cc.  */

  if (flag_dump_fortran_global)
    gfc_dump_global_symbols (stdout);

  gfc_end_source_files ();
  return true;

duplicate_main:
  /* If we see a duplicate main program, shut down.  If the second
     instance is an implied main program, i.e. data decls or executable
     statements, we're in for lots of errors.  */
  gfc_error ("Two main PROGRAMs at %L and %C", &prog_locus);
  reject_statement ();
  gfc_done_2 ();
  return true;
}

/* Return true if this state data represents an OpenACC region.  */
bool
is_oacc (gfc_state_data *sd)
{
  switch (sd->construct->op)
    {
    case EXEC_OACC_PARALLEL_LOOP:
    case EXEC_OACC_PARALLEL:
    case EXEC_OACC_KERNELS_LOOP:
    case EXEC_OACC_KERNELS:
    case EXEC_OACC_SERIAL_LOOP:
    case EXEC_OACC_SERIAL:
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_LOOP:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OACC_ATOMIC:
    case EXEC_OACC_ROUTINE:
      return true;

    default:
      return false;
    }
}
