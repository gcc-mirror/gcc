/* Implement tasking-related actions for CHILL.
   Copyright (C) 1992, 93, 1994, 1998, 1999 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "ch-tree.h"
#include "flags.h"
#include "input.h"
#include "obstack.h"
#include "assert.h"
#include "tasking.h"
#include "lex.h"
#include "toplev.h"

/* from ch-lex.l, from compiler directives */
extern tree process_type;
extern tree send_signal_prio;
extern tree send_buffer_prio;

tree tasking_message_type;
tree instance_type_node;
tree generic_signal_type_node;

/* the type a tasking code variable has */
tree chill_taskingcode_type_node;

/* forward declarations */
#if 0
static void validate_process_parameters		PARAMS ((tree));
static tree get_struct_variable_name		PARAMS ((tree));
static tree decl_tasking_code_variable		PARAMS ((tree, tree *, int));
#endif
static tree get_struct_debug_type_name		PARAMS ((tree));
static tree get_process_wrapper_name		PARAMS ((tree));
static tree build_tasking_enum			PARAMS ((void));
static void build_tasking_message_type		PARAMS ((void));
static tree build_receive_signal_case_label	PARAMS ((tree, tree));
static tree build_receive_buffer_case_label	PARAMS ((tree, tree));
static void build_receive_buffer_case_end	PARAMS ((tree, tree));
static void build_receive_signal_case_end	PARAMS ((tree, tree));

/* list of this module's process, buffer, etc. decls.
 This is a list of TREE_VECs, chain by their TREE_CHAINs. */
tree tasking_list = NULL_TREE;
/* The parts of a tasking_list element. */
#define TASK_INFO_PDECL(NODE) TREE_VEC_ELT(NODE,0)
#define TASK_INFO_ENTRY(NODE) TREE_VEC_ELT(NODE,1)
#define TASK_INFO_CODE_DECL(NODE) TREE_VEC_ELT(NODE,2)
#define TASK_INFO_STUFF_NUM(NODE) TREE_VEC_ELT(NODE,3)
#define TASK_INFO_STUFF_TYPE(NODE) TREE_VEC_ELT(NODE,4)

/* name template for process argument type */
#define STRUCT_NAME "__tmp_%s_arg_type"

/* name template for process arguments for debugging type */
#define STRUCT_DEBUG_NAME "__tmp_%s_debug_type"

/* name template for process argument variable */
#define DATA_NAME  "__tmp_%s_arg_variable"

/* name template for process wrapper */
#define WRAPPER_NAME "__tmp_%s_wrapper"

/* name template for process code */
#define SKELNAME "__tmp_%s_code"

extern int ignoring;
static tree void_ftype_void;
static tree pointer_to_instance;
static tree infinite_buffer_event_length_node;

tree
get_struct_type_name (name)
     tree name;
{
  const char *idp = IDENTIFIER_POINTER (name);        /* process name */
  char *tmpname = xmalloc (strlen (idp) + sizeof (STRUCT_NAME));

  sprintf (tmpname, STRUCT_NAME, idp);
  return get_identifier (tmpname);
}

static tree
get_struct_debug_type_name (name)
     tree name;
{
  const char *idp = IDENTIFIER_POINTER (name);        /* process name */
  char *tmpname = xmalloc (strlen (idp) + sizeof (STRUCT_DEBUG_NAME));

  sprintf (tmpname, STRUCT_DEBUG_NAME, idp);
  return get_identifier (tmpname);
}


tree
get_tasking_code_name (name)
     tree name;
{
  const char *name_str = IDENTIFIER_POINTER (name);
  char *tmpname  = (char *) alloca (IDENTIFIER_LENGTH (name) +
				    sizeof (SKELNAME));
  
  sprintf (tmpname, SKELNAME, name_str);
  return get_identifier (tmpname);
}

#if 0
static tree
get_struct_variable_name (name)
     tree name;
{
  const char *idp = IDENTIFIER_POINTER (name);        /* process name */
  char *tmpname = xmalloc (strlen (idp) + sizeof (DATA_NAME));

  sprintf (tmpname, DATA_NAME, idp);
  return get_identifier (tmpname);
}
#endif

static tree
get_process_wrapper_name (name)
    tree name;
{
  const char *idp = IDENTIFIER_POINTER (name);
  char *tmpname = xmalloc (strlen (idp) + sizeof (WRAPPER_NAME));
    
  sprintf (tmpname, WRAPPER_NAME, idp);
  return get_identifier (tmpname);
}

/*
 * If this is a quasi declaration - parsed within a SPEC MODULE,
 * QUASI_FLAG is TRUE, to indicate that the variable should not
 * be initialized.  The other module will do that.
 */
tree
generate_tasking_code_variable (name, tasking_code_ptr, quasi_flag)
     tree name, *tasking_code_ptr;
     int  quasi_flag;
{

  tree decl;
  tree tasking_code_name = get_tasking_code_name (name);
  
  if (pass == 2 && ! quasi_flag && *tasking_code_ptr != NULL_TREE)
    {
      /* check for value should be assigned is out of range */
      if (TREE_INT_CST_LOW (*tasking_code_ptr) >
	  TREE_INT_CST_LOW (TYPE_MAX_VALUE (chill_taskingcode_type_node)))
	  error ("Tasking code %ld out of range for `%s'.",
		 (long) TREE_INT_CST_LOW (*tasking_code_ptr),
		 IDENTIFIER_POINTER (name));
    }

  decl = do_decl (tasking_code_name, 
		  chill_taskingcode_type_node, 1, 1,
		  quasi_flag ? NULL_TREE : *tasking_code_ptr, 
		  0);

  /* prevent granting of this type */
  DECL_SOURCE_LINE (decl) = 0;

  if (pass == 2 && ! quasi_flag && *tasking_code_ptr != NULL_TREE)
    *tasking_code_ptr = fold (build (PLUS_EXPR, chill_taskingcode_type_node,
				     integer_one_node,
				     *tasking_code_ptr));
  return decl;
}


/*
 * If this is a quasi declaration - parsed within a SPEC MODULE,
 * QUASI_FLAG is TRUE, to indicate that the variable should not
 * be initialized.  The other module will do that.  This is just 
 * for BUFFERs and EVENTs.
 */
#if 0
static tree
decl_tasking_code_variable (name, tasking_code_ptr, quasi_flag)
     tree name, *tasking_code_ptr;
     int  quasi_flag;
{
  extern struct obstack permanent_obstack;
  tree tasking_code_name = get_tasking_code_name (name);
  tree decl;

  /* guarantee that RTL for the code_variable resides in
     the permanent obstack.  The BUFFER or EVENT may be
     declared in a PROC, not at global scope... */
  push_obstacks (&permanent_obstack, &permanent_obstack);
  push_obstacks_nochange ();

  if (pass == 2 && ! quasi_flag && *tasking_code_ptr != NULL_TREE)
    {
      /* check for value should be assigned is out of range */
      if (TREE_INT_CST_LOW (*tasking_code_ptr) >
	  TREE_INT_CST_LOW (TYPE_MAX_VALUE (chill_taskingcode_type_node)))
	  error ("Tasking code %ld out of range for `%s'.",
		 (long) TREE_INT_CST_LOW (*tasking_code_ptr),
		 IDENTIFIER_POINTER (name));
    }

  decl = decl_temp1 (tasking_code_name, 
		     chill_taskingcode_type_node, 1,
		     quasi_flag ? NULL_TREE : *tasking_code_ptr, 
		     0, 0);
  /* prevent granting of this type */
  DECL_SOURCE_LINE (decl) = 0;

  /* Return to the ambient context.  */
  pop_obstacks ();

  if (pass == 2 && ! quasi_flag && *tasking_code_ptr != NULL_TREE)
    *tasking_code_ptr = fold (build (PLUS_EXPR, chill_taskingcode_type_node,
				     integer_one_node,
				     *tasking_code_ptr));
  return decl;
}
#endif

/*
 * Transmute a process parameter list into an argument structure 
 * TYPE_DECL for the start_process call to reference.  Create a 
 * proc_type variable for later.  Returns the new struct type.
 */
tree
make_process_struct (name, processparlist)
     tree name, processparlist;
{
  tree temp;
  tree a_parm;
  tree field_decls = NULL_TREE;

  if (name == NULL_TREE || TREE_CODE (name) == ERROR_MARK)
    return error_mark_node;

  if (processparlist == NULL_TREE)
    return tree_cons (NULL_TREE, NULL_TREE, void_list_node);

  if (TREE_CODE (processparlist) == ERROR_MARK)
    return error_mark_node;

  /* build list of field decls for build_chill_struct_type */
  for (a_parm = processparlist; a_parm != NULL_TREE; 
       a_parm = TREE_CHAIN (a_parm))
    {
      tree parnamelist = TREE_VALUE (a_parm);
      tree purpose     = TREE_PURPOSE (a_parm);
      tree mode        = TREE_VALUE (purpose);
      tree parm_attr   = TREE_PURPOSE (purpose);
      tree field;

      /* build a FIELD_DECL node */
      if (parm_attr != NULL_TREE)
	{
	  if (parm_attr == ridpointers[(int)RID_LOC])
	    mode = build_chill_reference_type (mode);
	  else if (parm_attr == ridpointers[(int)RID_IN])
	    ;
	  else if (pass == 1)
	    {
	      for (field = parnamelist; field != NULL_TREE;
		   field = TREE_CHAIN (field))
	        error ("invalid attribute for argument `%s' (only IN or LOC allowed).",
		       IDENTIFIER_POINTER (TREE_VALUE (field)));
	    }
	}

      field = grok_chill_fixedfields (parnamelist, mode, NULL_TREE);

      /* chain the fields in reverse */
      if (field_decls == NULL_TREE)
	field_decls = field;
      else
	chainon (field_decls, field);
    }

  temp = build_chill_struct_type (field_decls);
  return temp;
}

/* Build a function for a PROCESS  and define some
   types for the process arguments.
   After the PROCESS a wrapper function will be 
   generated which gets the PROCESS arguments via a pointer
   to a structure having the same layout as the arguments.
   This wrapper function then will call the PROCESS.
   The advantage in doing it this way is, that PROCESS
   arguments may be displayed by gdb without any change
   to gdb.
*/
tree
build_process_header (plabel, paramlist)
     tree plabel, paramlist;
{
  tree struct_ptr_type = NULL_TREE;
  tree new_param_list = NULL_TREE;
  tree struct_decl = NULL_TREE;
  tree process_struct = NULL_TREE;
  tree struct_debug_type = NULL_TREE;
  tree code_decl;
    
  if (! global_bindings_p ())
    {
      error ("PROCESS may only be declared at module level");
      return error_mark_node;
    }

  if (paramlist)
    {
      /* must make the structure OUTSIDE the parameter scope */
      if (pass == 1)
	{
	  process_struct = make_process_struct (plabel, paramlist);
	  struct_ptr_type = build_chill_pointer_type (process_struct);
	}
      else
	{
	  process_struct = NULL_TREE;
	  struct_ptr_type = NULL_TREE;
	}
		          
      struct_decl = push_modedef (get_struct_type_name (plabel),
				  struct_ptr_type, -1);
      DECL_SOURCE_LINE (struct_decl) = 0;
      struct_debug_type = push_modedef (get_struct_debug_type_name (plabel),
					process_struct, -1);
      DECL_SOURCE_LINE (struct_debug_type) = 0;

      if (pass == 2)
        {
          /* build a list of PARM_DECL's */
          tree  wrk = paramlist;
          tree  tmp, list = NULL_TREE;
          
          while (wrk != NULL_TREE)
            {
              tree wrk1 = TREE_VALUE (wrk);
                
              while (wrk1 != NULL_TREE)
                {
                  tmp = make_node (PARM_DECL);
                  DECL_ASSEMBLER_NAME (tmp) = DECL_NAME (tmp) = TREE_VALUE (wrk1);
                  if (list == NULL_TREE)
                    new_param_list = list = tmp;
                  else
                    {
                      TREE_CHAIN (list) = tmp;
                      list = tmp;
                    }
                  wrk1 = TREE_CHAIN (wrk1);
                }
              wrk = TREE_CHAIN (wrk);
            }
        }
      else
        {
          /* build a list of modes */
          tree  wrk = paramlist;
          
          while (wrk != NULL_TREE)
            {
              tree wrk1 = TREE_VALUE (wrk);
              
              while (wrk1 != NULL_TREE)
                {
                  new_param_list = tree_cons (TREE_PURPOSE (TREE_PURPOSE (wrk)),
                                              TREE_VALUE (TREE_PURPOSE (wrk)),
                                              new_param_list);
                  wrk1 = TREE_CHAIN (wrk1);
                }
              wrk = TREE_CHAIN (wrk);
            }
          new_param_list = nreverse (new_param_list);
        }
    }

  /* declare the code variable outside the process */
  code_decl = generate_tasking_code_variable (plabel, 
					      &process_type, 0);

  /* start the parameter scope */
  push_chill_function_context ();

  if (! start_chill_function (plabel, void_type_node, 
			      new_param_list, NULL_TREE, NULL_TREE))
    return error_mark_node;

  current_module->procedure_seen = 1; 
  CH_DECL_PROCESS (current_function_decl) = 1;
  /* remember the code variable in the function decl */
  DECL_TASKING_CODE_DECL (current_function_decl) = 
    (struct lang_decl *)code_decl;
  if (paramlist == NULL_TREE)
      /* do it here, cause we don't have a wrapper */
    add_taskstuff_to_list (code_decl, "_TT_Process", process_type,
			   current_function_decl, NULL_TREE);

  return perm_tree_cons (code_decl, struct_decl, NULL_TREE);
}

/* Generate a function which gets a pointer
   to an argument block and call the corresponding
   PROCESS
*/
void
build_process_wrapper (plabel, processdata)
    tree        plabel;
    tree        processdata;
{
  tree  args = NULL_TREE;
  tree  wrapper = NULL_TREE;
  tree  parammode = TREE_VALUE (processdata);
  tree  code_decl = TREE_PURPOSE (processdata);
  tree  func = lookup_name (plabel);
    
  /* check the mode. If it is an ERROR_MARK there was an error
     in build_process_header, if it is a NULL_TREE the process
     don't have parameters, so we must not generate a wrapper */
  if (parammode == NULL_TREE ||
      TREE_CODE (parammode) == ERROR_MARK)
    return;
    
  /* get the function name */
  wrapper = get_process_wrapper_name (plabel);
    
  /* build the argument */
  if (pass == 2)
    {
      /* build a PARM_DECL */
      args = make_node (PARM_DECL);
      DECL_ASSEMBLER_NAME (args) = DECL_NAME (args) = get_identifier ("x");
    }
  else
    {
      /* build a tree list with the mode */
      args = tree_cons (NULL_TREE,
                        TREE_TYPE (parammode),
                        NULL_TREE);
    }
    
  /* start the function */
  push_chill_function_context ();
    
  if (! start_chill_function (wrapper, void_type_node,
                              args, NULL_TREE, NULL_TREE))
    return;

  /* to avoid granting */
  DECL_SOURCE_LINE (current_function_decl) = 0;

  if (! ignoring)
    {
      /* make the call to the PROCESS */
      tree      wrk;
      tree      x = lookup_name (get_identifier ("x"));
      /* no need to check this pointer to be NULL */
      tree      indref = build_chill_indirect_ref (x, NULL_TREE, 0);
        
      args = NULL_TREE;
      wrk = TYPE_FIELDS (TREE_TYPE (TREE_TYPE (x)));
      while (wrk != NULL_TREE)
        {
          args = tree_cons (NULL_TREE,
                            build_component_ref (indref, DECL_NAME (wrk)),
                            args);
          wrk = TREE_CHAIN (wrk);
        }
      CH_DECL_PROCESS (func) = 0;
      expand_expr_stmt (
        build_chill_function_call (func, nreverse (args)));
      CH_DECL_PROCESS (func) = 1;
    }

  add_taskstuff_to_list (code_decl, "_TT_Process", process_type,
                         func, current_function_decl);
    
  /* finish the function */
  finish_chill_function ();
  pop_chill_function_context (); 
}

/* Generate errors for INOUT, OUT parameters.

   "Only if LOC is specified may the mode have the non-value
    property"
 */

#if 0
static void
validate_process_parameters (parms)
     tree parms ATTRIBUTE_UNUSED;
{
}
#endif

/*
 * build the tree for a start process action.  Loop through the
 * actual parameters, making a constructor list, which we use to
 * initialize the argument structure.  NAME is the process' name.
 * COPYNUM is its copy number, whatever that is.  EXPRLIST is the
 * list of actual parameters passed by the start call.  They must
 * match. EXPRLIST must still be in reverse order;  we'll reverse it here.
 *
 * Note: the OPTSET name is not now used - it's here for 
 * possible future support for the optional 'SET instance-var'
 * clause.
 */
void
build_start_process (process_name, copynum,
		     exprlist, optset)
     tree process_name, copynum, exprlist, optset;
{
  tree process_decl = NULL_TREE, struct_type_node = NULL_TREE;
  tree result;
  tree valtail, typetail;
  tree tuple = NULL_TREE, actuallist = NULL_TREE;
  tree typelist;
  int  parmno = 2;
  tree args;
  tree filename, linenumber;
  
  if (exprlist != NULL_TREE && TREE_CODE (exprlist) == ERROR_MARK)
    process_decl = NULL_TREE;
  else if (! ignoring)
    {
      process_decl = lookup_name (process_name);
      if (process_decl == NULL_TREE)
	error ("process name %s never declared",
	       IDENTIFIER_POINTER (process_name));
      else if (TREE_CODE (process_decl) != FUNCTION_DECL
	  || ! CH_DECL_PROCESS (process_decl))
	{
	  error ("You may only START a process, not a proc");
	  process_decl = NULL_TREE;
	}
      else if (DECL_EXTERNAL (process_decl))
        {
          args = TYPE_ARG_TYPES (TREE_TYPE (process_decl));
          if (TREE_VALUE (args) != void_type_node)
              struct_type_node = TREE_TYPE (TREE_VALUE (args));
          else
              struct_type_node = NULL_TREE;
        }
      else
        {
          tree  debug_type = lookup_name (
                               get_struct_debug_type_name (DECL_NAME (process_decl)));

          if (debug_type == NULL_TREE)
              /* no debug type, no arguments */
              struct_type_node = NULL_TREE;
          else
              struct_type_node = TREE_TYPE (debug_type);
        }
    }

  /* begin a new name scope */
  pushlevel (1);
  clear_last_expr ();
  push_momentary ();
  if (pass == 2)
    expand_start_bindings (0);

  if (! ignoring && process_decl != NULL_TREE)
    {
      if (optset == NULL_TREE) ;
      else if (!CH_REFERABLE (optset))
	{
	  error ("SET expression not a location.");
	  optset = NULL_TREE;
	}
      else if (!CH_IS_INSTANCE_MODE (TREE_TYPE (optset)))
	{
	  error ("SET location must be INSTANCE mode");
	  optset = NULL_TREE;
	}
      if (optset)
	optset = force_addr_of (optset);
      else
	optset = convert (ptr_type_node, integer_zero_node);

      if (struct_type_node != NULL_TREE)
	{
	  typelist = TYPE_FIELDS (struct_type_node);

	  for (valtail = nreverse (exprlist), typetail = typelist;
	       valtail != NULL_TREE && typetail != NULL_TREE;  parmno++,
	       valtail = TREE_CHAIN (valtail), typetail = TREE_CHAIN (typetail))
	    {
	      register tree actual  = valtail  ? TREE_VALUE (valtail)  : 0;
	      register tree type    = typetail ? TREE_TYPE (typetail) : 0;
	      char place[30];
	      sprintf (place, "signal field %d", parmno);
	      actual = chill_convert_for_assignment (type, actual, place);
	      actuallist = tree_cons (NULL_TREE, actual, 
				      actuallist);
	    }

	  tuple = build_nt (CONSTRUCTOR, NULL_TREE, 
			    nreverse (actuallist));
	}
      else
	{
	  valtail = NULL_TREE;
	  typetail = NULL_TREE;
	}
  
      if (valtail != 0 && TREE_VALUE (valtail) != void_type_node)
	{
	  if (process_name)
	    error ("too many arguments to process `%s'",
		   IDENTIFIER_POINTER (process_name));
	  else
	    error ("too many arguments to process");
	}
      else if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
	{
	  if (process_name)
	    error ("too few arguments to process `%s'",
		   IDENTIFIER_POINTER (process_name));
	  else
	    error ("too few arguments to process");
	}
      else
      {
	tree process_decl = lookup_name (process_name);
	tree process_type = (tree)DECL_TASKING_CODE_DECL (process_decl);
	tree struct_size, struct_pointer;
	
	if (struct_type_node != NULL_TREE)
	  {
	    result = 
	      decl_temp1 (get_unique_identifier ("START_ARG"),
			  struct_type_node, 0, tuple, 0, 0);
	    /* prevent granting of this type */
	    DECL_SOURCE_LINE (result) = 0;

	    mark_addressable (result);
	    struct_pointer
	      = build1 (ADDR_EXPR,
			build_chill_pointer_type (struct_type_node),
			result);
	    struct_size = size_in_bytes (struct_type_node);
	  }
	else
	  {
	    struct_size = integer_zero_node;
	    struct_pointer = null_pointer_node;
	  }

	filename = force_addr_of (get_chill_filename ());
	linenumber = get_chill_linenumber ();
	
	expand_expr_stmt (
          build_chill_function_call (lookup_name (get_identifier ("__start_process")),
	    tree_cons (NULL_TREE, process_type,
              tree_cons (NULL_TREE, convert (integer_type_node, copynum),
		tree_cons (NULL_TREE, struct_size,
		  tree_cons (NULL_TREE, struct_pointer,
		    tree_cons (NULL_TREE, optset,
		      tree_cons (NULL_TREE, filename,
		        build_tree_list (NULL_TREE, linenumber)))))))));
      }
    }
  /* end of scope */

  if (pass == 2)
    expand_end_bindings (getdecls (), kept_level_p (), 0);
  poplevel (kept_level_p (), 0, 0);
  pop_momentary ();
}

/*
 * A CHILL SET which represents all of the possible tasking
 * elements.
 */
static tree
build_tasking_enum ()
{
  tree result, decl1;
  tree enum1;
  tree list = NULL_TREE;
  tree value = integer_zero_node;

  enum1  = start_enum (NULL_TREE);
  result = build_enumerator (get_identifier ("_TT_UNUSED"),
			     value);
  list = chainon (result, list);
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));
		      
  result = build_enumerator (get_identifier ("_TT_Process"),
			     value);
  list = chainon (result, list);
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));
		      
  result = build_enumerator (get_identifier ("_TT_Signal"),
			     value);
  list = chainon (result, list);
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));

  result = build_enumerator (get_identifier ("_TT_Buffer"),
			     value);
  list = chainon (result, list);
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));
  
  result = build_enumerator (get_identifier ("_TT_Event"),
			     value);
  list = chainon (result, list);
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));

  result = build_enumerator (get_identifier ("_TT_Synonym"),
			     value);
  list = chainon (result, list);
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));
  
  result = build_enumerator (get_identifier ("_TT_Exception"),
			     value);
  list = chainon (result, list);  
  value = fold (build (PLUS_EXPR, integer_type_node,
		       value, integer_one_node));

  result = finish_enum (enum1, list); 

  decl1 = build_decl (TYPE_DECL, 
		      get_identifier ("__tmp_TaskingEnum"),
		      result);
  pushdecl (decl1);
  satisfy_decl (decl1, 0);
  return decl1;
}

tree
build_tasking_struct ()
{  
  tree listbase, decl1, decl2, result;
  tree enum_type = TREE_TYPE (build_tasking_enum ());
  /* We temporarily reset the maximum_field_alignment to zero so the
     compiler's init data structures can be compatible with the
     run-time system, even when we're compiling with -fpack. */
  extern int maximum_field_alignment;
  int save_maximum_field_alignment = maximum_field_alignment;
  maximum_field_alignment = 0;

  decl1 = build_decl (FIELD_DECL, get_identifier ("TaskName"),
		      build_chill_pointer_type (char_type_node));
  DECL_INITIAL (decl1) = NULL_TREE;
  listbase = decl1;

  decl2 = build_decl (FIELD_DECL, get_identifier ("TaskValue"),
		      build_chill_pointer_type (chill_taskingcode_type_node));
  TREE_CHAIN (decl1) = decl2;
  DECL_INITIAL (decl2) = NULL_TREE;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL, get_identifier ("TaskValueDefined"),
                      integer_type_node);
  TREE_CHAIN (decl1) = decl2;
  DECL_INITIAL (decl2) = NULL_TREE;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL, get_identifier ("TaskEntry"),
                      build_chill_pointer_type (void_ftype_void));
  TREE_CHAIN (decl1) = decl2;
  DECL_INITIAL (decl2) = NULL_TREE;
  decl1 = decl2;

  decl2 = build_decl (FIELD_DECL, get_identifier ("TaskType"),
		      enum_type);
  TREE_CHAIN (decl1) = decl2;
  DECL_INITIAL (decl2) = NULL_TREE;
  decl1 = decl2;

  TREE_CHAIN (decl2) = NULL_TREE;
  result = build_chill_struct_type (listbase);
  satisfy_decl (result, 0);
  maximum_field_alignment = save_maximum_field_alignment;
  return result;
}

/*
 * build data structures describing each task/signal, etc.
 * in current module.
 */
void
tasking_setup ()
{
  tree tasknode;
  tree struct_type;

  if (pass == 1)
    return;

  struct_type = TREE_TYPE (lookup_name (
		  get_identifier ("__tmp_TaskingStruct")));

  for (tasknode = tasking_list; tasknode != NULL_TREE; 
       tasknode = TREE_CHAIN (tasknode))
    {
      /* This is the tasking_code_variable's decl */
      tree stuffnumber = TASK_INFO_STUFF_NUM (tasknode);
      tree code_decl   = TASK_INFO_CODE_DECL (tasknode);
      tree proc_decl   = TASK_INFO_PDECL (tasknode);
      tree entry       = TASK_INFO_ENTRY (tasknode);
      tree name = DECL_NAME (proc_decl);
      char *init_struct = (char *) alloca (IDENTIFIER_LENGTH(name) + 20);
      /* take care of zero termination */
      tree task_name;
      /* these are the fields of the struct, in declaration order */
      tree init_flag = (stuffnumber == NULL_TREE) ? 
	integer_zero_node : integer_one_node;
      tree type = DECL_INITIAL (TASK_INFO_STUFF_TYPE (tasknode));
      tree int_addr;
      tree entry_point;
      tree name_ptr;
      tree decl;
      tree struct_id;
      tree initializer;
      
      if (TREE_CODE (proc_decl) == FUNCTION_DECL
	  && CH_DECL_PROCESS (proc_decl) 
	  && ! DECL_EXTERNAL (proc_decl))
        {
          if (entry == NULL_TREE)
	    entry = proc_decl;
	  mark_addressable (entry);
	  entry_point = build1 (ADDR_EXPR, 
				build_chill_pointer_type (void_ftype_void),
				entry);
        }
      else
	entry_point = build1 (NOP_EXPR, 
                        build_chill_pointer_type (void_ftype_void), 
                          null_pointer_node);

      /* take care of zero termination */
      task_name = 
	build_chill_string (IDENTIFIER_LENGTH (name) + 1,
			    IDENTIFIER_POINTER (name));

      mark_addressable (code_decl);
      int_addr = build1 (ADDR_EXPR,
			 build_chill_pointer_type (chill_integer_type_node),
			 code_decl);

      mark_addressable (task_name);
      name_ptr = build1 (ADDR_EXPR,
		   build_chill_pointer_type (char_type_node), 
		     task_name);

      sprintf (init_struct, "__tmp_%s_struct", 
	       IDENTIFIER_POINTER (name));

      struct_id = get_identifier (init_struct);
      initializer = build (CONSTRUCTOR, struct_type, NULL_TREE,
		      tree_cons (NULL_TREE, name_ptr,
                        tree_cons (NULL_TREE, int_addr,
                          tree_cons (NULL_TREE, init_flag,
                            tree_cons (NULL_TREE, entry_point,
                              tree_cons (NULL_TREE, type, NULL_TREE))))));
      TREE_CONSTANT (initializer) = 1;
      decl = decl_temp1 (struct_id, struct_type, 1, initializer, 0, 0);
      /* prevent granting of this type */
      DECL_SOURCE_LINE (decl) = 0;

      /* pass the decl to tasking_registry() in the symbol table */
      IDENTIFIER_LOCAL_VALUE (struct_id) = decl;
    }
}


/*
 * Generate code to register the tasking-related stuff
 * with the runtime.  Only in pass 2.
 */
void
tasking_registry ()
{
  tree tasknode, fn_decl;

  if (pass == 1)
    return;

  fn_decl = lookup_name (get_identifier ("__register_tasking"));

  for (tasknode = tasking_list; tasknode != NULL_TREE; 
       tasknode = TREE_CHAIN (tasknode))
    {
      tree proc_decl = TASK_INFO_PDECL (tasknode);
      tree name = DECL_NAME (proc_decl);
      tree arg_decl;
      char *init_struct = (char *) alloca (IDENTIFIER_LENGTH (name) + 20);

      sprintf (init_struct, "__tmp_%s_struct", 
	       IDENTIFIER_POINTER (name));
      arg_decl = lookup_name (get_identifier (init_struct));

      expand_expr_stmt (
        build_chill_function_call (fn_decl,
	  build_tree_list (NULL_TREE, force_addr_of (arg_decl))));
    }
}

/*
 * Put a tasking entity (a PROCESS, or SIGNAL) onto
 * the list for tasking_setup (). CODE_DECL is the integer code
 * variable's DECL, which describes the shadow integer which 
 * accompanies each tasking entity.  STUFFTYPE is a string
 * representing the sort of tasking entity we have here (i.e. 
 * process, signal, etc.).  STUFFNUMBER is an enumeration
 * value saying the same thing.  PROC_DECL is the declaration of
 * the entity.  It's a FUNCTION_DECL if the entity is a PROCESS, it's
 * a TYPE_DECL if the entity is a SIGNAL.
 */
void
add_taskstuff_to_list (code_decl, stufftype, stuffnumber,
		       proc_decl, entry)
     tree code_decl;
     const char *stufftype;
     tree stuffnumber, proc_decl, entry;
{
  if (pass == 1)
    /* tell chill_finish_compile that there's
       task-level code to be processed. */
    tasking_list = integer_one_node;

  /* do only in pass 2 so we know in chill_finish_compile whether
     to generate a constructor function, and to avoid double the
     correct number of entries. */
  else /* pass == 2 */
    {
      tree task_node = make_tree_vec (5);
      TASK_INFO_PDECL (task_node) = proc_decl;
      TASK_INFO_ENTRY (task_node) = entry;
      TASK_INFO_CODE_DECL (task_node) = code_decl;
      TASK_INFO_STUFF_NUM (task_node) = stuffnumber;
      TASK_INFO_STUFF_TYPE (task_node)
	= lookup_name (get_identifier (stufftype));
      TREE_CHAIN (task_node) = tasking_list;
      tasking_list = task_node;
    }
}

/*
 * These next routines are called out of build_generalized_call
 */
tree
build_copy_number (instance_expr)
     tree instance_expr;
{
  tree result;

  if (instance_expr == NULL_TREE 
      || TREE_CODE (instance_expr) == ERROR_MARK)
    return error_mark_node;
  if (! CH_IS_INSTANCE_MODE (TREE_TYPE (instance_expr)))
    {
      error ("COPY_NUMBER argument must be INSTANCE expression");
      return error_mark_node;
    }
  result = build_component_ref (instance_expr,
				get_identifier (INS_COPY));
  CH_DERIVED_FLAG (result) = 1;
  return result;
}


tree
build_gen_code (decl)
     tree decl;
{
  tree result;

  if (decl == NULL_TREE || TREE_CODE (decl) == ERROR_MARK)
    return error_mark_node;

  if ((TREE_CODE (decl) == FUNCTION_DECL && CH_DECL_PROCESS (decl))
      || (TREE_CODE (decl) == TYPE_DECL && CH_DECL_SIGNAL (decl)))
    result = (tree)(DECL_TASKING_CODE_DECL (decl));
  else
    {
      error ("GEN_CODE argument must be a process or signal name.");
      return error_mark_node;
    }
  CH_DERIVED_FLAG (result) = 1;
  return (result);
}


tree
build_gen_inst (process, copyn)
     tree process, copyn;
{
  tree ptype;
  tree result;

  if (copyn == NULL_TREE || TREE_CODE (copyn) == ERROR_MARK)
    return error_mark_node;
  if (process == NULL_TREE || TREE_CODE (process) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (TREE_TYPE (copyn)) != INTEGER_TYPE)
    {
      error ("GEN_INST parameter 2 must be an integer mode");
      copyn = integer_zero_node;
    }

  copyn = check_range (copyn, copyn, 
		       TYPE_MIN_VALUE (chill_taskingcode_type_node),
		       TYPE_MAX_VALUE (chill_taskingcode_type_node));

  if (TREE_CODE (process) == FUNCTION_DECL
      && CH_DECL_PROCESS (process))
    ptype = (tree)DECL_TASKING_CODE_DECL (process);
  else if (TREE_TYPE (process) != NULL_TREE
	   && TREE_CODE (TREE_TYPE (process)) == INTEGER_TYPE)
    {
      process = check_range (process, process, 
			     TYPE_MIN_VALUE (chill_taskingcode_type_node),
			     TYPE_MAX_VALUE (chill_taskingcode_type_node));
      ptype = convert (chill_taskingcode_type_node, process);
    }
  else
    {
      error ("GEN_INST parameter 1 must be a PROCESS or an integer expression");
      return (error_mark_node);
    }
  
  result = convert (instance_type_node,
	     build_nt (CONSTRUCTOR, NULL_TREE,
	       tree_cons (NULL_TREE, ptype,
	         tree_cons (NULL_TREE, 
	           convert (chill_taskingcode_type_node, copyn), NULL_TREE))));
  CH_DERIVED_FLAG (result) = 1;
  return result;
}


tree
build_gen_ptype (process_decl)
     tree process_decl;
{
  tree result;

  if (process_decl == NULL_TREE || TREE_CODE (process_decl) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (process_decl) != FUNCTION_DECL
      || ! CH_DECL_PROCESS (process_decl))
    {
      error_with_decl (process_decl, "%s is not a declared process");
      return error_mark_node;
    }

  result = (tree)DECL_TASKING_CODE_DECL (process_decl);
  CH_DERIVED_FLAG (result) = 1;
  return result;
}


tree
build_proc_type (instance_expr)
     tree instance_expr;
{
  tree result;

  if (instance_expr == NULL_TREE || TREE_CODE (instance_expr) == ERROR_MARK)
    return error_mark_node;

  if (! CH_IS_INSTANCE_MODE (TREE_TYPE (instance_expr)))
    {
      error ("PROC_TYPE argument must be INSTANCE expression");
      return error_mark_node;
    }
  result = build_component_ref (instance_expr,
				get_identifier (INS_PTYPE));
  CH_DERIVED_FLAG (result) = 1;
  return result;
}

tree
build_queue_length (buf_ev)
     tree buf_ev;
{
  if (buf_ev == NULL_TREE || TREE_CODE (buf_ev) == ERROR_MARK)
    return error_mark_node;
  if (TREE_TYPE (buf_ev) == NULL_TREE ||
      TREE_CODE (TREE_TYPE (buf_ev)) == ERROR_MARK)
    return error_mark_node;

  if (CH_IS_BUFFER_MODE (TREE_TYPE (buf_ev)) ||
      CH_IS_EVENT_MODE (TREE_TYPE (buf_ev)))
    {
      const char *field_name;
      tree  arg1, arg2;

      if (CH_IS_EVENT_MODE (TREE_TYPE (buf_ev)))
	{
	  field_name = "__event_data";
	  arg2 = integer_one_node;
	}
      else
	{
	  field_name = "__buffer_data";
	  arg2 = integer_zero_node;
	}
      arg1 = build_component_ref (buf_ev, get_identifier (field_name));
      return build_chill_function_call (
                lookup_name (get_identifier ("__queue_length")),
                   tree_cons (NULL_TREE, arg1,
                      tree_cons (NULL_TREE, arg2, NULL_TREE)));
    }

  error ("QUEUE_LENGTH argument must be a BUFFER/EVENT location.");
  return error_mark_node;
}

tree
build_signal_struct_type (signame, sigmodelist, optsigdest)
     tree signame, sigmodelist, optsigdest;
{
  tree decl, temp;

  if (pass == 1)
    {
      int  fldcnt = 0;
      tree mode, field_decls = NULL_TREE;

      for (mode = sigmodelist; mode != NULL_TREE; mode = TREE_CHAIN (mode))
	{ 
	  tree field;
	  char fldname[20];
      
	  if (TREE_VALUE (mode) == NULL_TREE)
	    continue;
	  sprintf (fldname, "fld%03d", fldcnt++);
	  field = build_decl (FIELD_DECL,
			      get_identifier (fldname),
			      TREE_VALUE (mode));
	  if (field_decls == NULL_TREE)
	    field_decls = field;
	  else
	    chainon (field_decls, field);
	}
      if (field_decls == NULL_TREE)
	field_decls = build_decl (FIELD_DECL,
				  get_identifier ("__tmp_empty"),
				  boolean_type_node); 
      temp = build_chill_struct_type (field_decls);

      /* save the destination process name of the signal */
      IDENTIFIER_SIGNAL_DEST (signame) = optsigdest;
      IDENTIFIER_SIGNAL_DATA (signame) = fldcnt;
    }
  else
    {
      /* optsigset is only valid in pass 2, so we have to save it now */
      IDENTIFIER_SIGNAL_DEST (signame) = optsigdest;
      temp = NULL_TREE; /* Actually, don't care. */
    }
  
  decl = push_modedef (signame, temp, -1);
  if (decl != NULL_TREE)
    CH_DECL_SIGNAL (decl) = 1;
  return decl;
}

/*
 * An instance type is a unique process identifier in the CHILL
 * tasking arena.  It consists of a process type and a copy number.
 */
void
build_instance_type ()
{
  tree decl1, decl2, tdecl;

  decl1 = build_decl (FIELD_DECL, get_identifier (INS_PTYPE), 
		      chill_taskingcode_type_node);

  TREE_CHAIN (decl1) = decl2 =
    build_decl (FIELD_DECL, get_identifier (INS_COPY), 
		chill_taskingcode_type_node);
  TREE_CHAIN (decl2) = NULL_TREE;

  instance_type_node = build_chill_struct_type (decl1);
  tdecl = build_decl (TYPE_DECL, ridpointers[(int) RID_INSTANCE],
		      instance_type_node);
  TYPE_NAME (instance_type_node) = tdecl;
  CH_NOVELTY (instance_type_node) = tdecl;
  DECL_SOURCE_LINE (tdecl) = 0;
  pushdecl (tdecl);

  pointer_to_instance = build_chill_pointer_type (instance_type_node);
}

#if 0
 *
 * The tasking message descriptor looks like this C structure:
 *
 * typedef struct
 *   {
 *     short *sc;                 /* ptr to code integer */
 *     int    data_len;           /* length of signal/buffer data msg */
 *     void  *data;               /* ptr to signal/buffer data */
 *   } SignalDescr;
 *
 *
#endif

static void
build_tasking_message_type ()
{
  tree type_name;
  tree temp;
  /* We temporarily reset maximum_field_alignment to deal with
     the runtime system. */
  extern int maximum_field_alignment;
  int save_maximum_field_alignment = maximum_field_alignment;
  tree field1, field2, field3;

  maximum_field_alignment = 0;
  field1 = build_decl (FIELD_DECL, 
		       get_identifier ("_SD_code_ptr"),
		       build_pointer_type (chill_integer_type_node));
  field2 = build_decl (FIELD_DECL,
		       get_identifier ("_SD_data_len"),
		       integer_type_node);
  field3 = build_decl (FIELD_DECL,
		       get_identifier ("_SD_data_ptr"),
		       ptr_type_node);
  TREE_CHAIN (field1) = field2;
  TREE_CHAIN (field2) = field3;
  temp = build_chill_struct_type (field1);
  
  type_name = get_identifier ("__tmp_SD_struct");
  tasking_message_type = build_decl (TYPE_DECL, type_name, temp);

  /* This won't get seen in pass 2, so lay it out now.  */
  layout_chill_struct_type (temp);
  pushdecl (tasking_message_type);
  maximum_field_alignment = save_maximum_field_alignment;
}

tree
build_signal_descriptor (sigdef, exprlist)
     tree sigdef, exprlist;
{
  tree fieldlist, typetail, valtail;
  tree actuallist = NULL_TREE;
  tree signame = DECL_NAME (sigdef);
  tree dataptr, datalen;
  int  parmno = 1;

  if (sigdef == NULL_TREE
      || TREE_CODE (sigdef) == ERROR_MARK)
    return error_mark_node;

  if (exprlist != NULL_TREE
      && TREE_CODE (exprlist) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (sigdef) != TYPE_DECL
      || ! CH_DECL_SIGNAL (sigdef))
    {
      error ("SEND requires a SIGNAL; %s is not a SIGNAL name", 
	     IDENTIFIER_POINTER (signame));
      return error_mark_node;
    }
  if (CH_TYPE_NONVALUE_P (TREE_TYPE (sigdef)))
    return error_mark_node;

  fieldlist = TYPE_FIELDS (TREE_TYPE (sigdef));
  if (IDENTIFIER_SIGNAL_DATA (signame) == 0)
    fieldlist = TREE_CHAIN (fieldlist);

  for (valtail = exprlist, typetail = fieldlist;
       valtail != NULL_TREE && typetail != NULL_TREE;  
       parmno++, valtail = TREE_CHAIN (valtail),
       typetail = TREE_CHAIN (typetail))
    {
      register tree actual  = valtail  ? TREE_VALUE (valtail) : 0;
      register tree type    = typetail ? TREE_TYPE (typetail) : 0;
      char place[30];
      sprintf (place, "signal field %d", parmno);
      actual = chill_convert_for_assignment (type, actual, place);
      actuallist = tree_cons (NULL_TREE,  actual, actuallist);
    }
  if (valtail != 0 && TREE_VALUE (valtail) != void_type_node)
    {
      error ("too many values for SIGNAL `%s'",
	     IDENTIFIER_POINTER (signame));
      return error_mark_node;
    }
  else if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
    {
      error ("too few values for SIGNAL `%s'",
	   IDENTIFIER_POINTER (signame));
      return error_mark_node;
    }

  {
    /* build signal data structure */
    tree sigdataname = get_unique_identifier (
                         IDENTIFIER_POINTER (signame));
    if (exprlist == NULL_TREE)
      {
	dataptr = null_pointer_node;
	datalen = integer_zero_node;
      }
    else
      {
	tree tuple = build_nt (CONSTRUCTOR,
		       NULL_TREE, nreverse (actuallist));
	tree decl = decl_temp1 (sigdataname, TREE_TYPE (sigdef), 
			   0, tuple, 0, 0);
	/* prevent granting of this type */
	DECL_SOURCE_LINE (decl) = 0;

	dataptr = force_addr_of (decl);
	datalen = size_in_bytes (TREE_TYPE (decl));
      }
    
    /* build descriptor pointing to signal data */
    {
      tree decl, tuple;
      tree tasking_message_var = get_unique_identifier (
                                   IDENTIFIER_POINTER (signame));

      tree tasking_code = 
	(tree)DECL_TASKING_CODE_DECL (lookup_name (signame));

      mark_addressable (tasking_code);
      tuple = build_nt (CONSTRUCTOR, NULL_TREE,
		tree_cons (NULL_TREE, 
		  build1 (ADDR_EXPR, 
		    build_chill_pointer_type (chill_integer_type_node), 
			  tasking_code),
		      tree_cons (NULL_TREE, datalen,
		        tree_cons (NULL_TREE, dataptr, NULL_TREE))));
			      
      decl = decl_temp1 (tasking_message_var,
			 TREE_TYPE (tasking_message_type), 0,
			 tuple, 0, 0);
      /* prevent granting of this type */
      DECL_SOURCE_LINE (decl) = 0;

      tuple = force_addr_of (decl);
      return tuple;
    }
  }
}

void
expand_send_signal (sigmsgbuffer, optroutinginfo, optsendto,
		   optpriority, signame)
     tree sigmsgbuffer;
     tree optroutinginfo;
     tree optsendto;
     tree optpriority;
     tree signame;
{
  tree routing_size, routing_addr;
  tree filename, linenumber;
  tree sigdest = IDENTIFIER_SIGNAL_DEST (signame);

  /* check the presence of priority */
  if (optpriority == NULL_TREE)
    {
      if (send_signal_prio == NULL_TREE)
	{
	  /* issue a warning in case of -Wall */
	  if (extra_warnings)
	    {
	      warning ("Signal sent without priority");
	      warning (" and no default priority was set.");
	      warning (" PRIORITY defaulted to 0");
	    }
	  optpriority = integer_zero_node;
	}
      else
	optpriority = send_signal_prio;
    }

  /* check the presence of a destination.
     optdest either may be an instance location
     or a process declaration */
  if (optsendto == NULL_TREE)
    {
      if (sigdest == NULL_TREE)
        {
	  error ("SEND without a destination instance");
	  error (" and no destination process specified");
	  error (" for the signal");
	  optsendto = convert (instance_type_node,
			       null_pointer_node);
        }
      else
        {
	  /* build an instance [sigdest; -1] */
	  tree process_name = DECL_NAME (sigdest);
	  tree copy_number = fold (build (MINUS_EXPR, integer_type_node,
					  integer_zero_node,
					  integer_one_node));
	  tree tasking_code = (tree)DECL_TASKING_CODE_DECL (
                                lookup_name (process_name));

	  optsendto = build (CONSTRUCTOR, instance_type_node, NULL_TREE,
                        tree_cons (NULL_TREE, tasking_code,
                          tree_cons (NULL_TREE, copy_number, NULL_TREE)));
	  /* as our system doesn't allow that and Z.200 specifies it,
	     we issue a warning */
	  warning ("SEND to ANY copy of process `%s'.", IDENTIFIER_POINTER (process_name));
        }
    }
  else if (! CH_IS_INSTANCE_MODE (TREE_TYPE (optsendto)))
    {
      error ("SEND TO must be an INSTANCE mode");
      optsendto = convert (instance_type_node, null_pointer_node);
    }
  else
    optsendto = check_non_null (convert (instance_type_node, optsendto));

  /* check the routing stuff */
  if (optroutinginfo != NULL_TREE)
    {
      tree routing_name;
      tree decl;

      if (TREE_TYPE (optroutinginfo) == NULL_TREE)
	{
	  error ("SEND WITH must have a mode");
	  optroutinginfo = integer_zero_node;
	}
      routing_name = get_unique_identifier ("RI");
      decl = decl_temp1 (routing_name,
			 TREE_TYPE (optroutinginfo), 0,
			 optroutinginfo, 0, 0);
      /* prevent granting of this type */
      DECL_SOURCE_LINE (decl) = 0;

      routing_addr = force_addr_of (decl);
      routing_size = size_in_bytes (TREE_TYPE (decl));
    }
  else
    {
      routing_size = integer_zero_node;
      routing_addr = null_pointer_node;
    }
  /* get filename and linenumber */
  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();
  
  /* Now (at last!) we can call the runtime */
  expand_expr_stmt (
    build_chill_function_call (lookup_name (get_identifier ("__send_signal")),
      tree_cons (NULL_TREE, sigmsgbuffer,
        tree_cons (NULL_TREE, optsendto,
	  tree_cons (NULL_TREE, optpriority,
	    tree_cons (NULL_TREE, routing_size,
	      tree_cons (NULL_TREE, routing_addr,
                tree_cons (NULL_TREE, filename,
		  tree_cons (NULL_TREE, linenumber, NULL_TREE)))))))));
}

#if 0
 * The following code builds a RECEIVE CASE action, which actually
 * has 2 different functionalities:
 *
 * 1) RECEIVE signal CASE action
 *   which looks like this:
 *
 *    SIGNAL advance;
 *    SIGNAL terminate = (CHAR);
 *    SIGNAL sig1 = (CHAR);
 *
 *    DCL user, system INSTANCE;
 *    DCL count INT, char_code CHAR;
 *    DCL instance_loc INSTANCE;
 *
 *    workloop:
 *      RECEIVE CASE SET instance_loc;
 *        (advance): 
 *           count + := 1;
 *        (terminate IN char_code):
 *           SEND sig1(char_code) TO system;
 *           EXIT workloop; 
 *      ELSE 
 *        STOP;
 *      ESAC;
 *
 * Because we don''t know until we get to the ESAC how
 * many signals need processing, we generate the following
 * C-equivalent code:
 *
 * /* define the codes for the signals */
 * static short __tmp_advance_code;
 * static short __tmp_terminate_code;
 * static short __tmp_sig1_code;
 *
 * /* define the types of the signals */
 * typedef struct
 *  {
 *     char fld0;
 *  } __tmp_terminate_struct;
 *
 * typedef struct
 *  {
 *     char fld0;
 *  } __tmp_sig1_struct;
 *
 * static INSTANCE user, system, instance_loc;
 * static short count;
 * static char char_code;
 *
 * {               /* start a new symbol context */
 *   int    number_of_sigs;
 *   short *sig_code [];
 *   void  *sigdatabuf;
 *   int    sigdatalen;
 *   short  sigcode;
 *
 *   goto __rcsetup;
 *
 *  __rcdoit: ;
 *   int timedout = __wait_signal (&sigcode
 *                                 number_of_sigs,
 *                                 sig_code,
 *                                 sigdatabuf,
 *                                 sigdatalen,
 *                                 &instance_loc);
 *   if (sigcode == __tmp_advance_code)
 *     {
 *       /* code for advance alternative's action_statement_list */
 *       count++;
 *     }
 *   else if (sigcode == __tmp_terminate_code)
 *     {
 *        /* copy signal's data to where they belong,
 *           with range-check, if enabled */
 *        char_code = ((__tmp_terminate_struct *)sigdatabuf)->fld0;
 *
 *       /* code for terminate alternative's action_statement_list */
 *        __send_signal (sig1 ..... );
 *        goto __workloop_end;
 *     }
 *   else
 *     {
 *        /* code here for the ELSE action_statement_list */
 *        __stop_process ();
 *     }
 *   goto __rc_done;
 *
 * __rcsetup:
 *   union { __tmp_terminate_struct terminate; 
 *	     __tmp_sig1_struct } databuf;
 *   short *sig_code_ptr [2] = { &__tmp_advance_code,
 *                               &__tmp_terminate_code };
 *   sigdatabuf = &databuf;
 *   sigdatalen = sizeof (databuf);
 *   sig_code = &sig_code_ptr[0];
 *   number_of_sigs = 2;
 *   goto __rcdoit;
 *
 * __rc_done: ;
 * }               /* end the new symbol context */
 * __workloop_end: ;
 *
 *
 * 2) RECEIVE buffer CASE action:
 *   which looks like this:
 *
 *    NEWMODE m_s = STRUCT (mini INT, maxi INT);
 *    DCL b1 BUFFER INT;
 *    DCL b2 BUFFER (30) s;
 *
 *    DCL i INT, s m_s, ins INSTANCE;
 *    DCL count INT;
 *
 *    workloop:
 *      RECEIVE CASE SET ins;
 *        (b1 IN i):
 *          count +:= i;
 *        (b2 in s):
 *          IF count < s.mini OR count > s.maxi THEN
 *            EXIT workloop;
 *          FI;
 *        ELSE
 *          STOP;
 *      ESAC;
 *
 * Because we don''t know until we get to the ESAC how
 * many buffers need processing, we generate the following
 * C-equivalent code:
 *
 * typedef struct
 * {
 *    short mini;
 *    short maxi;
 * } m_s;
 *
 * static void *b1;
 * static void *b2;
 * static short i;
 * static m_s s;
 * static INSTANCE ins;
 * static short count;
 *
 * workloop:
 * {                     /* start a new symbol context */
 *   int     number_of_sigs;
 *   void   *sig_code [];
 *   void   *sigdatabuf;
 *   int     sigdatalen;
 *   void   *buflocation;
 *   int     timedout;
 *
 *   goto __rcsetup;
 *
 *  __rcdoit:
 *   timedout = __wait_buffer (&buflocation,
 *                             number_of_sigs,
 *                             sig_code,
 *                             sigdatabuf,
 *                             sigdatalen,
 *                             &ins, ...);
 *   if (buflocation == &b1)
 *     {
 *       i = ((short *)sigdatabuf)->fld0;
 *       count += i;
 *     }
 *   else if (buflocation == &b2)
 *     {
 *       s = ((m_s)*sigdatabuf)->fld1;
 *       if (count < s.mini || count > s.maxi)
 *         goto __workloop_end;
 *     }
 *   else
 *       __stop_process ();
 *   goto __rc_done;
 *
 *  __rcsetup:
 *   typedef struct
 *   {
 *      void      *p;
 *      unsigned   maxqueuesize;
 *   } Buffer_Descr;
 *   union { short    b1,
 *           m_s      b2 } databuf;
 *   Buffer_Descr bufptr [2] =
 *       {
 *         { &b1, -1 },
 *         { &b2, 30 },
 *       };
 *   void * bufarray[2] = { &bufptr[0],
 *                          &bufptr[1] };
 *   sigdatabuf = &databuf;
 *   sigdatalen = sizeof (databuf);
 *   sig_code = &bufarray[0];
 *   number_of_sigs = 2;
 *   goto __rcdoit;
 *
 *  __rc_done;
 * }          /* end of symbol context */
 * __workloop_end:
 *
#endif

struct rc_state_type
{
  struct rc_state_type *enclosing;
  rtx  rcdoit;
  rtx  rcsetup;
  tree n_sigs;
  tree sig_code;
  tree databufp;
  tree datalen;
  tree else_clause;
  tree received_signal;
  tree received_buffer;
  tree to_loc;
  int  sigseen;
  int  bufseen;
  tree actuallist;
  int  call_generated;
  int  if_generated;
  int  bufcnt;
};

struct rc_state_type *current_rc_state = NULL;

/* 
 * this function tells if there is an if to terminate
 * or not
 */
int
build_receive_case_if_generated()
{
  if (!current_rc_state)
    {
      error ("internal error: RECEIVE CASE stack invalid.");
      abort ();
    }
  return current_rc_state->if_generated;
}

/* build_receive_case_start returns an INTEGER_CST node
   containing the case-label number to be used by
   build_receive_case_end to generate correct labels */
tree
build_receive_case_start (optset)
     tree optset;
{
  /* counter to generate unique receive_case labels */
  static int rc_lbl_count = 0;
  tree current_label_value = 
    build_int_2 ((HOST_WIDE_INT)rc_lbl_count, 0);
  tree sigcodename, filename, linenumber;
  
  struct rc_state_type *rc_state
    = (struct rc_state_type*) xmalloc (sizeof (struct rc_state_type));
  rc_state->rcdoit = gen_label_rtx ();
  rc_state->rcsetup = gen_label_rtx ();
  rc_state->enclosing = current_rc_state;
  current_rc_state = rc_state;
  rc_state->sigseen = 0;
  rc_state->bufseen = 0;
  rc_state->call_generated = 0;
  rc_state->if_generated = 0;
  rc_state->bufcnt = 0;

  rc_lbl_count++;
  if (optset == NULL_TREE || TREE_CODE (optset) == ERROR_MARK)
    optset = null_pointer_node;
  else
    {
      if (CH_IS_INSTANCE_MODE (TREE_TYPE (optset)) && CH_LOCATION_P (optset))
	optset = force_addr_of (optset);
      else
	{
	  error ("SET requires INSTANCE location");
	  optset = null_pointer_node;
	}			 
    }

  rc_state->to_loc = build_timeout_preface ();
  
  rc_state->n_sigs =
    decl_temp1 (get_identifier ("number_of_sigs"),
		integer_type_node, 0, integer_zero_node, 0, 0);

  rc_state->sig_code =
    decl_temp1 (get_identifier ("sig_codep"),
		ptr_type_node, 0, null_pointer_node, 0, 0);

  rc_state->databufp =
    decl_temp1 (get_identifier ("databufp"),
		ptr_type_node, 0, null_pointer_node, 0, 0);

  rc_state->datalen =
    decl_temp1 (get_identifier ("datalen"),
		integer_type_node, 0, integer_zero_node, 0, 0);

  rc_state->else_clause =
    decl_temp1 (get_identifier ("else_clause"),
		integer_type_node, 0, integer_zero_node, 0, 0);

  /* wait_signal will store the signal number in here */
  sigcodename = get_identifier ("received_signal");
  rc_state->received_signal = 
    decl_temp1 (sigcodename, chill_integer_type_node, 0, 
		NULL_TREE, 0, 0);

  /* wait_buffer will store the buffer address in here */
  sigcodename = get_unique_identifier ("received_buffer");
  rc_state->received_buffer =
    decl_temp1 (sigcodename, ptr_type_node, 0,
		NULL_TREE, 0, 0);

  /* now jump to the end of RECEIVE CASE actions, to
     set up variables for them. */
  emit_jump (rc_state->rcsetup);

  /* define the __rcdoit label. We come here after
     initialization of all variables, to execute the
     actions. */
  emit_label (rc_state->rcdoit);

  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();
  
  /* Argument list for calling the runtime routine.  We'll call it
     the first time we call build_receive_case_label, when we know
     whether to call wait_signal or wait_buffer. NOTE: at this time
     the first argument will be set. */
  rc_state->actuallist = 
    tree_cons (NULL_TREE, NULL_TREE,
      tree_cons (NULL_TREE, rc_state->n_sigs,
        tree_cons (NULL_TREE, rc_state->sig_code,
          tree_cons (NULL_TREE, rc_state->databufp,
            tree_cons (NULL_TREE, rc_state->datalen,
              tree_cons (NULL_TREE, optset, 
	        tree_cons (NULL_TREE, rc_state->else_clause,
	          tree_cons (NULL_TREE, rc_state->to_loc,
		    tree_cons (NULL_TREE, filename,
		      tree_cons (NULL_TREE, linenumber, NULL_TREE))))))))));
  return current_label_value;
}

static tree
build_receive_signal_case_label (sigdecl, loclist)
     tree sigdecl, loclist;
{
  struct rc_state_type *rc_state = current_rc_state;
  tree signame = DECL_NAME (sigdecl);
  tree expr;

  if (rc_state->bufseen != 0)
    {
      error ("SIGNAL in RECEIVE CASE alternative follows");
      error (" a BUFFER name on line %d", rc_state->bufseen);
      return error_mark_node;
    }
  rc_state->sigseen = lineno;
  rc_state->bufseen = 0;

  if (!IDENTIFIER_SIGNAL_DATA (signame) && loclist != NULL_TREE)
    {
      error ("SIGNAL `%s' has no data fields", IDENTIFIER_POINTER (signame));
      return error_mark_node;
    }
  if (IDENTIFIER_SIGNAL_DATA (signame) && loclist == NULL_TREE)
    {
      error ("SIGNAL `%s' requires data fields", IDENTIFIER_POINTER (signame));
      return error_mark_node;
    }

  if (!rc_state->call_generated)
    {
      tree wait_call;

      TREE_VALUE (rc_state->actuallist) = force_addr_of (rc_state->received_signal);
      wait_call = build_chill_function_call (lookup_name
		    (get_identifier ("__wait_signal_timed")),
		       rc_state->actuallist);
#if 0
      chill_expand_assignment (rc_state->received_signal,
			       NOP_EXPR, wait_call);
#endif
      build_timesupervised_call (wait_call, rc_state->to_loc);
      
      rc_state->call_generated = 1;
    }

  /* build the conditional expression */
  expr = build (EQ_EXPR, boolean_type_node,
		rc_state->received_signal,
		(tree)DECL_TASKING_CODE_DECL (sigdecl));

  if (!rc_state->if_generated)
    {
      expand_start_cond (expr, 0);
      rc_state->if_generated = 1;
    }
  else
    expand_start_elseif (expr);

  if (IDENTIFIER_SIGNAL_DATA (signame))
    {
      /* copy data from signal buffer to user's variables */
      tree typelist = TYPE_FIELDS (TREE_TYPE (sigdecl));
      tree valtail, typetail;
      int  parmno = 1;
      tree pointer_type = build_chill_pointer_type (TREE_TYPE (sigdecl));
      tree pointer = convert (pointer_type, rc_state->databufp);
	  
      for (valtail = nreverse (loclist), typetail = typelist;
	   valtail != NULL_TREE && typetail != NULL_TREE;  
	   parmno++, valtail = TREE_CHAIN (valtail),
	   typetail = TREE_CHAIN (typetail))
	{
	  register tree actual  = valtail  ? TREE_VALUE (valtail)  : 0;
	  register tree type    = typetail ? TREE_TYPE (typetail) : 0;
	  register tree assgn;
	  char place[30];
	  sprintf (place, "signal field %d", parmno);

	  assgn = build_component_ref (build1 (INDIRECT_REF,
					       TREE_TYPE (sigdecl),
					       pointer),
				       DECL_NAME (typetail));
	  if (!CH_TYPE_NONVALUE_P (type))
	    /* don't assign to non-value type. Error printed at signal definition */
	    chill_expand_assignment (actual, NOP_EXPR, assgn);
	}

      if (valtail == NULL_TREE && typetail != NULL_TREE)
	error ("too few data fields provided for `%s'",
	       IDENTIFIER_POINTER (signame));
      if (valtail != NULL_TREE && typetail == NULL_TREE)
	error ("too many data fields provided for `%s'",
	       IDENTIFIER_POINTER (signame));
    }

  /* last action here */
  emit_line_note (input_filename, lineno);

  return build_tree_list (loclist, signame);
}

static tree
build_receive_buffer_case_label (buffer, loclist)
     tree buffer, loclist;
{
  struct rc_state_type *rc_state = current_rc_state;
  tree buftype = buffer_element_mode (TREE_TYPE (buffer));
  tree expr, var;
  tree pointer_type, pointer, assgn;
  int  had_errors = 0;
  tree x, y, z, bufaddr;

  if (rc_state->sigseen != 0)
    {
      error ("BUFFER in RECEIVE CASE alternative follows");
      error (" a SIGNAL name on line %d", rc_state->sigseen);
      return error_mark_node;
    }
  rc_state->bufseen = lineno;
  rc_state->sigseen = 0;

  if (! CH_REFERABLE (buffer))
    {
      error ("BUFFER in RECEIVE CASE alternative must be a location.");
      return error_mark_node;
    }

  if (TREE_CHAIN (loclist) != NULL_TREE)
    {
      error ("buffer receive alternative requires only 1 defining occurence.");
      return error_mark_node;
    }

  if (!rc_state->call_generated)
    {
      tree wait_call;

      /* here we change the mode of rc_state->sig_code to
	 REF ARRAY (0:65535) REF __tmp_DESCR_type.
	 This is neccesary, cause we cannot evaluate the buffer twice
	 (once here where we compare against the address of the buffer
	 and second in build_receive_buffer_case_end, where we use the
	 address build the descriptor, which gets passed to __wait_buffer).
	 So we change the comparison from
	 if (rc_state->received_buffer == &buffer)
	 to
	 if (rc_state->received_buffer ==
	 rc_state->sig_codep->[rc_state->bufcnt]->datap).
	 
	 This will evaluate the buffer location only once
	 (in build_receive_buffer_case_end) and therefore doesn't confuse
	 our machinery. */
      
      tree reftmpdescr = build_chill_pointer_type (
		            TREE_TYPE (lookup_name (
				get_identifier ("__tmp_DESCR_type"))));
      tree idxtype = build_chill_range_type (NULL_TREE,
			integer_zero_node,
			   build_int_2 (65535, 0)); /* should be enough, probably use ULONG */
      tree arrtype = build_chill_array_type (reftmpdescr,
			tree_cons (NULL_TREE, idxtype, NULL_TREE),
			   0, NULL_TREE);
      tree refarrtype = build_chill_pointer_type (arrtype);

      TREE_VALUE (rc_state->actuallist) = force_addr_of (rc_state->received_buffer);
      wait_call = build_chill_function_call (
		    lookup_name (get_identifier ("__wait_buffer")),
		      rc_state->actuallist);
#if 0
      chill_expand_assignment (rc_state->received_buffer,
				 NOP_EXPR, wait_call);
#endif
      build_timesupervised_call (wait_call, rc_state->to_loc);
      
      /* do this after the call, otherwise there will be a mode mismatch */
      TREE_TYPE (rc_state->sig_code) = refarrtype;
      
      /* now we are ready to generate the call */
      rc_state->call_generated = 1;
    }

  x = build_chill_indirect_ref (rc_state->sig_code, NULL_TREE, 0);
  y = build_chill_array_ref (x,
        tree_cons (NULL_TREE, build_int_2 (rc_state->bufcnt, 0), NULL_TREE));
  z = build_chill_indirect_ref (y, NULL_TREE, 0);
  bufaddr = build_chill_component_ref (z, get_identifier ("datap"));

  /* build the conditional expression */
  expr = build (EQ_EXPR, boolean_type_node,
		rc_state->received_buffer,
		bufaddr);

  /* next buffer in list */
  rc_state->bufcnt++;

  if (!rc_state->if_generated)
    {
      expand_start_cond (expr, 0);
      rc_state->if_generated = 1;
    }
  else
    expand_start_elseif (expr);

  /* copy buffer's data to destination */
  var = TREE_VALUE (loclist);

  if (buftype != NULL_TREE && TREE_CODE (buftype) == ERROR_MARK)
    had_errors = 1;
  else if (! CH_COMPATIBLE (var, buftype))
    {
      error ("incompatible modes in receive buffer alternative.");
      had_errors = 1;
    }

  if (! CH_LOCATION_P (var))
    {
      error ("defining occurence in receive buffer alternative must be a location.");
      had_errors = 1;
    }

  if (! had_errors)
    {
      pointer_type = build_chill_pointer_type (TREE_TYPE (var));
      pointer = convert (pointer_type,
			 rc_state->databufp);
      /* no need to check this pointer being NULL */
      assgn = build_chill_indirect_ref (pointer, NULL_TREE, 0);
      
      chill_expand_assignment (var, NOP_EXPR, assgn);
    }

  /* last action here */
  emit_line_note (input_filename, lineno);

  return build_tree_list (loclist, buffer);
}
/*
 *  SIGNAME is the signal name or buffer location,
 *  LOCLIST is a list of possible locations to store data in
 */
tree
build_receive_case_label (signame, loclist)
     tree signame, loclist;
{
  /* now see what we have got and do some checks */
  if (TREE_CODE (signame) == TYPE_DECL && CH_DECL_SIGNAL (signame))
    return build_receive_signal_case_label (signame, loclist);

  if (TREE_TYPE (signame) != NULL_TREE
      && CH_IS_BUFFER_MODE (TREE_TYPE (signame)))
    {
      if (loclist == NULL_TREE)
	{
	  error ("buffer receive alternative without `IN location'.");
	  return error_mark_node;
	}
      return build_receive_buffer_case_label (signame, loclist);
    }

  error ("RECEIVE CASE alternative must specify a SIGNAL name or BUFFER location.");
  return error_mark_node;
}

/*
 * LABEL_CNT is the case-label counter passed from build_receive_case_start.
 * ELSE_CLAUSE defines if the RECEIVE CASE action had an ELSE(1) or not(0).
 * BUF_LIST is a tree-list of tree-lists, where TREE_VALUE defines the 
 * BUFFER location and TREE_PURPOSE defines the defining occurence.
 */
static void
build_receive_buffer_case_end (buf_list, else_clause)
     tree buf_list, else_clause;
{
  struct rc_state_type *rc_state = current_rc_state;
  tree alist;
  tree field_decls = NULL_TREE; /* list of all buffer types, for the union */
  int  buffer_cnt = 0;
  tree descr_type = lookup_name (get_identifier ("__tmp_DESCR_type"));
  tree tuple = NULL_TREE;       /* constructors for array of ptrs */
  tree union_type_node = NULL_TREE;

  /* walk thru all the buffers */
  for (alist = buf_list; alist != NULL_TREE;
       buffer_cnt++, alist = TREE_CHAIN (alist))
    {
      tree value      = TREE_VALUE (alist);
      tree buffer     = TREE_VALUE (value);                 /* this is the buffer */
      tree data       = TREE_VALUE (TREE_PURPOSE (value));  /* the location to receive in */
      tree buffer_descr;
      tree buffer_descr_init;
      tree buffer_length;
      tree field;
      char fldname[20];

      /* build descriptor for buffer */
      buffer_length = max_queue_size (TREE_TYPE (buffer));
      if (buffer_length == NULL_TREE)
	buffer_length = infinite_buffer_event_length_node;
      buffer_descr_init = build_nt (CONSTRUCTOR, NULL_TREE,
                            tree_cons (NULL_TREE, force_addr_of (buffer),
                              tree_cons (NULL_TREE, buffer_length, NULL_TREE)));
      buffer_descr = decl_temp1 (get_unique_identifier ("RCbuffer"),
				 TREE_TYPE (descr_type), 0,
				 buffer_descr_init, 0, 0);
      tuple = tree_cons (NULL_TREE,
			 force_addr_of (buffer_descr),
			 tuple);

      /* make a field for the union */
      sprintf (fldname, "fld%03d", buffer_cnt);
      field = grok_chill_fixedfields (
                 tree_cons (NULL_TREE, get_identifier (fldname), NULL_TREE),
                   TREE_TYPE (data), NULL_TREE);
      if (field_decls == NULL_TREE)
	field_decls = field;
      else
	chainon (field_decls, field);
    }

  /* generate the union */
  if (field_decls != NULL_TREE)
    {
      tree data_id = get_identifier ("databuffer");
      tree data_decl;

      union_type_node = finish_struct (
			  start_struct (UNION_TYPE, NULL_TREE),
			    field_decls);
      data_decl = decl_temp1 (data_id, union_type_node, 0, NULL_TREE, 0, 0);

      chill_expand_assignment (rc_state->databufp, NOP_EXPR,
			       force_addr_of (data_decl));

      chill_expand_assignment (rc_state->datalen, NOP_EXPR,
			       size_in_bytes (TREE_TYPE (data_decl)));
    }

  /* tell runtime system if we had an else or not */
  chill_expand_assignment (rc_state->else_clause, NOP_EXPR, else_clause);

  /* generate the array of pointers to all buffers */
  {
    tree array_id = get_identifier ("buf_ptr_array");
    tree array_type_node =
           build_chill_array_type (ptr_type_node,
             tree_cons (NULL_TREE,
               build_chill_range_type (NULL_TREE,
				       integer_one_node,
				       build_int_2 (buffer_cnt, 0)),
			NULL_TREE),
			  0, NULL_TREE);
    tree constr = build_nt (CONSTRUCTOR, NULL_TREE, nreverse (tuple));
    tree array_decl = decl_temp1 (array_id, array_type_node, 0,
				  constr, 0, 0);
    
    chill_expand_assignment (build_chill_cast (ptr_type_node, rc_state->sig_code),
			     NOP_EXPR,
			     force_addr_of (array_decl));
    chill_expand_assignment (rc_state->n_sigs, NOP_EXPR,
			     build_int_2 (buffer_cnt, 0));
  }
}

/*
 * SIG_LIST is a tree list.  The TREE_VALUEs are VAR_DECLs of 
 * __tmp_%s_code variables, and the TREE_PURPOSEs are the
 * TYPE_DECLs of the __tmp_%s_struct types.  LABEL_CNT is the
 * case-label counter passed from build_receive_case_start.
 */
static void
build_receive_signal_case_end (sig_list, else_clause)
     tree sig_list, else_clause;
{
  struct rc_state_type *rc_state = current_rc_state;
  tree alist, temp1;
  tree union_type_node = NULL_TREE;
  tree field_decls = NULL_TREE;  /* list of signal
				   structure, for the union */
  tree tuple = NULL_TREE;    /* constructor for array of ptrs */
  int  signal_cnt = 0;
  int  fldcnt = 0;

  /* for each list of locations, validate it against the
     corresponding signal's list of fields. */
  {
    for (alist = sig_list; alist != NULL_TREE;
	 signal_cnt++, alist = TREE_CHAIN (alist))
      {
	tree value    = TREE_VALUE (alist);
	tree signame  = TREE_VALUE (value);  /* signal's ID node */
	tree sigdecl  = lookup_name (signame);
	tree sigtype  = TREE_TYPE (sigdecl);
	tree field;
	char fldname[20];

	if (IDENTIFIER_SIGNAL_DATA (signame))
	  {
	    sprintf (fldname, "fld%03d", fldcnt++);
	    field = grok_chill_fixedfields (
		      tree_cons (NULL_TREE, 
				 get_identifier (fldname),
				 NULL_TREE),
			sigtype, NULL_TREE); 
	    if (field_decls == NULL_TREE)
	      field_decls = field;
	    else
	      chainon (field_decls, field);

	  }

	temp1 = (tree)DECL_TASKING_CODE_DECL (sigdecl);
	mark_addressable (temp1);
	tuple = tree_cons (NULL_TREE,
		  build1 (ADDR_EXPR, 
		    build_chill_pointer_type (chill_integer_type_node),
			  temp1),
		    tuple);
      }
  }

  /* generate the union of all of the signal data types */
  if (field_decls != NULL_TREE)
    {
      tree data_id = get_identifier ("databuffer");
      tree data_decl;
      union_type_node = finish_struct (start_struct (UNION_TYPE, 
					       NULL_TREE),
				 field_decls); 
      data_decl =
	decl_temp1 (data_id, union_type_node, 0, NULL_TREE, 0, 0);

      chill_expand_assignment (rc_state->databufp, NOP_EXPR,
			       force_addr_of (data_decl));

      chill_expand_assignment (rc_state->datalen, NOP_EXPR, 
			       size_in_bytes (TREE_TYPE (data_decl)));
    }

  /* tell runtime system if we had an else or not */
  chill_expand_assignment (rc_state->else_clause, NOP_EXPR, else_clause);

  /* generate the array of all signal codes */
  {
    tree array_id = get_identifier ("sig_code_array");
    tree array_type_node
      = build_chill_array_type (
          build_chill_pointer_type (chill_integer_type_node),
	    tree_cons (NULL_TREE,
	      build_chill_range_type (NULL_TREE,
				      integer_one_node,
				      build_int_2 (signal_cnt, 0)),
		       NULL_TREE),
	 0, NULL_TREE);
    tree constr = build_nt (CONSTRUCTOR, NULL_TREE,
			    nreverse (tuple));
    tree array_decl = 
      decl_temp1 (array_id, array_type_node, 0, constr, 0, 0);

    chill_expand_assignment (rc_state->sig_code, NOP_EXPR, 
			     force_addr_of (array_decl));

    /* give number of signals to runtime system */
    chill_expand_assignment (rc_state->n_sigs, NOP_EXPR, 
			     build_int_2 (signal_cnt, 0));
  }
}

/* General function for the end of a RECEIVE CASE action */

void
build_receive_case_end (alist, else_clause)
     tree alist, else_clause;
{
  rtx rcdone = gen_label_rtx ();
  struct rc_state_type *rc_state = current_rc_state;
  tree tmp;
  int had_errors = 0;

  /* finish the if's, if generated */
  if (rc_state->if_generated)
    expand_end_cond ();

  /* check alist for errors */
  for (tmp = alist; tmp != NULL_TREE; tmp = TREE_CHAIN (tmp))
    {
      if (TREE_CODE (TREE_VALUE (tmp)) == ERROR_MARK)
	had_errors++;
    }

  /* jump to the end of RECEIVE CASE processing */
  emit_jump (rcdone);

  /* define the __rcsetup label. We come here to initialize
     all variables */
  emit_label (rc_state->rcsetup);

  if (alist == NULL_TREE && !had_errors)
    {
      error ("RECEIVE CASE without alternatives");
      goto gen_rcdoit;
    }

  if (TREE_CODE (alist) == ERROR_MARK || had_errors)
    goto gen_rcdoit;

  /* now call the actual end function */
  if (rc_state->bufseen)
    build_receive_buffer_case_end (alist, else_clause);
  else
    build_receive_signal_case_end (alist, else_clause);

  /* now jump to the beginning of RECEIVE CASE processing */
gen_rcdoit: ;
  emit_jump (rc_state->rcdoit);

  /* define the __rcdone label. We come here when the whole
     receive case is done. */
  emit_label (rcdone);

  current_rc_state = rc_state->enclosing;
  free(rc_state);
}

/* build a CONTINUE action */

void expand_continue_event (evloc)
     tree evloc;
{
  tree filename, linenumber, evaddr;

  /* do some checks */
  if (evloc == NULL_TREE || TREE_CODE (evloc) == ERROR_MARK)
    return;

  if (! CH_REFERABLE (evloc) || ! CH_IS_EVENT_MODE (TREE_TYPE (evloc)))
    {
      error ("CONTINUE requires an event location.");
      return;
    }

  evaddr = force_addr_of (evloc);
  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();

  expand_expr_stmt (
    build_chill_function_call (lookup_name (get_identifier ("__continue")),
      tree_cons (NULL_TREE, evaddr,
        tree_cons (NULL_TREE, filename,
          tree_cons (NULL_TREE, linenumber, NULL_TREE)))));
}

#if 0
 * The following code builds a DELAY CASE statement,
 * which looks like this in CHILL:
 *
 *    DCL ev1, ev2 EVENT, ins INSTANCE;
 *    DCL ev3 EVENT (10);
 *    DCL count1 INT := 0, count2 INT := 0;
 *
 *    DELAY CASE SET ins;
 *      (ev1): count1 +:= 1;
 *      (ev2, ev3): count2 +:= 1;
 *    ESAC; 
 *
 * Because we don''t know until we get to the ESAC how
 * many events need processing, we generate the following
 * C-equivalent code:
 *
 *
 * {               /* start a new symbol context */
 *   typedef struct
 *   {
 *      void           *p;
 *      unsigned long  len;
 *   } Descr;
 *   int     number_of_events;
 *   Descr  *event_codes;
 *
 *   goto __dlsetup;
 *
 *  __dldoit: 
 *   void *whatevent = __delay_event (number_of_events,
 *                                    event_codes,
 *                                    priority,
 *                                    &instance_loc,
 *                                    filename,
 *                                    linenumber);
 *   if (whatevent == &ev1)
 *     {
 *       /* code for ev1 alternative's action_statement_list */
 *       count1 += 1;
 *     }
 *   else if (whatevent == &ev2 || whatevent == &ev3)
 *     {
 *       /* code for ev2 and ev3 alternative's action_statement_list */
 *       count2 += 1;
 *     }
 *   goto __dl_done;
 *
 * __dlsetup:
 *   Descr event_code_ptr [3] = {
 *              { &ev1, -1 },
 *              { &ev2, -1 },
 *              { &ev3, 10 } };
 *   event_codes = &event_code_ptr[0];
 *   number_of_events = 3;
 *   goto __dldoit;
 *
 * __dl_done: 
 *   ;
 * }               /* end the new symbol context */
 *
#endif

struct dl_state_type
{
  struct dl_state_type *enclosing;
  rtx  dldoit;
  rtx  dlsetup;
  tree n_events;
  tree event_codes;
  tree received_event;
};

struct dl_state_type *current_dl_state = NULL;

/* build_receive_case_start returns an INTEGER_CST node
   containing the case-label number to be used by
   build_receive_case_end to generate correct labels */
tree
build_delay_case_start (optset, optpriority)
     tree optset, optpriority;
{
  /* counter to generate unique delay case labels */
  static int dl_lbl_count = 0;
  tree current_label_value = 
    build_int_2 ((HOST_WIDE_INT)dl_lbl_count, 0);
  tree wait_call;
  tree actuallist = NULL_TREE;
  tree filename, linenumber;
  tree to_loc;
  
  struct dl_state_type *dl_state
    = (struct dl_state_type*) xmalloc (sizeof (struct dl_state_type));
  dl_state->enclosing = current_dl_state;
  current_dl_state = dl_state;
  dl_state->dldoit = gen_label_rtx ();
  dl_state->dlsetup = gen_label_rtx ();

  dl_lbl_count++;

  /* check the optional SET location */
  if (optset == NULL_TREE
      || TREE_CODE (optset) == ERROR_MARK)
    optset = null_pointer_node;
  else if (CH_IS_INSTANCE_MODE (TREE_TYPE (optset)) && CH_LOCATION_P (optset))
    optset = force_addr_of (optset);
  else
    {
      error ("SET requires INSTANCE location");
      optset = null_pointer_node;
    }			 

  /* check the presence of the PRIORITY expression */
  if (optpriority == NULL_TREE)
    optpriority = integer_zero_node;
  else if (TREE_CODE (optpriority) == ERROR_MARK)
    optpriority = integer_zero_node;
  else if (TREE_CODE (TREE_TYPE (optpriority)) != INTEGER_TYPE)
    {
      error ("PRIORITY must be of integer type.");
      optpriority = integer_zero_node;
    }

  /* check for time supervised */
  to_loc = build_timeout_preface ();
  
  dl_state->n_events =
    decl_temp1 (get_identifier ("number_of_events"),
		integer_type_node, 0, integer_zero_node, 0, 0);

  dl_state->event_codes =
    decl_temp1 (get_identifier ("event_codes"),
		ptr_type_node, 0, null_pointer_node, 0, 0);

  /* wait_event will store the signal number in here */
  dl_state->received_event =
    decl_temp1 (get_identifier ("received_event"),
		ptr_type_node, 0, NULL_TREE, 0, 0);

  /* now jump to the end of RECEIVE CASE actions, to
     set up variables for them. */
  emit_jump (dl_state->dlsetup);

  /* define the __rcdoit label. We come here after
     initialization of all variables, to execute the
     actions. */
  emit_label (dl_state->dldoit);

  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();
  
  /* here we go, call the runtime routine */
  actuallist = tree_cons (NULL_TREE, force_addr_of (dl_state->received_event),
                 tree_cons (NULL_TREE, dl_state->n_events,
                   tree_cons (NULL_TREE, dl_state->event_codes,
                     tree_cons (NULL_TREE, optpriority, 
                       tree_cons (NULL_TREE, to_loc,
                         tree_cons (NULL_TREE, optset, 
		           tree_cons (NULL_TREE, filename,
		             tree_cons (NULL_TREE, linenumber, NULL_TREE))))))));

  wait_call = build_chill_function_call (
                lookup_name (get_identifier ("__delay_event")),
					 actuallist);

#if 0
  chill_expand_assignment (dl_state->received_event, NOP_EXPR, wait_call);
#endif
  build_timesupervised_call (wait_call, to_loc);
  return current_label_value;
}

/*
   EVENTLIST is the list of this alternative's events
   and IF_OR_ELSEIF indicates what action (1 for if and 
   0 for else if) should be generated.
*/
void
build_delay_case_label (eventlist, if_or_elseif)
     tree eventlist;
     int  if_or_elseif;
{
  tree eventp, expr = NULL_TREE;

  if (eventlist == NULL_TREE || TREE_CODE (eventlist) == ERROR_MARK)
    return;

  for (eventp = eventlist; eventp != NULL_TREE; 
       eventp = TREE_CHAIN (eventp))
    {
      tree event = TREE_VALUE (eventp);
      tree temp1;

      if (event == NULL_TREE || TREE_CODE (event) == ERROR_MARK)
	temp1 = null_pointer_node;
      else if (! CH_IS_EVENT_MODE (TREE_TYPE (event)) || ! CH_REFERABLE (event))
	{
	  error ("delay alternative must be an EVENT location.");
	  temp1 = null_pointer_node;
	}
      else
	temp1 = force_addr_of (event);
      
      /* build the conditional expression */
      if (expr == NULL_TREE)
	expr = build (EQ_EXPR, boolean_type_node,
		      current_dl_state->received_event, temp1);
      else
	expr = 
	  build (TRUTH_ORIF_EXPR, boolean_type_node, expr,
		 build (EQ_EXPR, boolean_type_node,
			current_dl_state->received_event, temp1));
    }
  if (if_or_elseif)
    expand_start_cond (expr, 0);
  else
    expand_start_elseif (expr);

  /* last action here */
  emit_line_note (input_filename, lineno);
}

/*
 * EVENT_LIST is a tree list.  The TREE_VALUEs are VAR_DECLs of 
 * EVENT variables.  LABEL_CNT is the case-label counter
 * passed from build_delay_case_start.
 */
void
build_delay_case_end (event_list)
     tree event_list;
{
  struct dl_state_type *dl_state = current_dl_state;
  rtx    dldone          = gen_label_rtx ();
  tree tuple = NULL_TREE;    /* constructor for array of descrs */
  tree acode;
  int  event_cnt = 0;

  /* if we have an empty event_list, there was no alternatives and we
     havn't started an if therefor don't run expand_end_cond */
  if (event_list != NULL_TREE)
    /* finish the if's */
    expand_end_cond ();

  /* jump to the end of RECEIVE CASE processing */
  emit_jump (dldone);

  /* define the __dlsetup label. We come here to initialize
     all variables */
  emit_label (dl_state->dlsetup);

  if (event_list == NULL_TREE)
    {
      error ("DELAY CASE without alternatives");
      goto gen_dldoit;
    }

  if (event_list == NULL_TREE 
      || TREE_CODE (event_list) == ERROR_MARK)
    goto gen_dldoit;

  /* make a list of pointers (in reverse order)
     to the event code variables */
  for (acode = event_list; acode != NULL_TREE; 
       acode = TREE_CHAIN (acode))
    {
      tree event = TREE_VALUE (acode);
      tree event_length;
      tree descr_init;

      if (event == NULL_TREE || TREE_CODE (event) == ERROR_MARK)
	{
	  descr_init = 
	    tree_cons (NULL_TREE, null_pointer_node,
	      tree_cons (NULL_TREE, integer_zero_node, NULL_TREE));
	}
      else
	{
	  event_length = max_queue_size (TREE_TYPE (event));
	  if (event_length == NULL_TREE)
	    event_length = infinite_buffer_event_length_node;
	  descr_init =
	    tree_cons (NULL_TREE, force_addr_of (event),
              tree_cons (NULL_TREE, event_length, NULL_TREE));
	}
      tuple = tree_cons (NULL_TREE,
		build_nt (CONSTRUCTOR, NULL_TREE, descr_init),
		  tuple);
      event_cnt++;
    }
    
  /* generate the array of all event code pointers */
  {
    tree descr_type = TREE_TYPE (lookup_name (get_identifier ("__tmp_DESCR_type")));
    tree array_id = get_identifier ("event_code_array");
    tree array_type_node
      = build_chill_array_type (descr_type,
	 tree_cons (NULL_TREE,
           build_chill_range_type (NULL_TREE,
				   integer_one_node,
				   build_int_2 (event_cnt, 0)),
		    NULL_TREE),
	 0, NULL_TREE);
    tree constr = build_nt (CONSTRUCTOR, NULL_TREE,
			    nreverse (tuple));
    tree array_decl = 
      decl_temp1 (array_id, array_type_node, 0, constr, 0, 0);

    chill_expand_assignment (dl_state->event_codes, NOP_EXPR, 
			     force_addr_of (array_decl));

    /* give number of signals to runtime system */
    chill_expand_assignment (dl_state->n_events, NOP_EXPR, 
			     build_int_2 (event_cnt, 0));
  }

  /* now jump to the beginning of DELAY CASE processing */
gen_dldoit: 
  emit_jump (dl_state->dldoit);

  /* define the __dldone label. We come here when the whole
     DELAY CASE is done. */
  emit_label (dldone);

  current_dl_state = dl_state->enclosing;
  free(dl_state);
}

#if 0
 * The following code builds a simple delay statement,
 * which looks like this in CHILL:
 *
 *    DCL ev1 EVENT(5), ins INSTANCE;
 *
 *    DELAY ev1 PRIORITY 7;
 *
 * This statement unconditionally delays the current 
 * PROCESS, until some other process CONTINUEs it.
 *
 * Here is the generated C code:
 *
 * typedef struct
 * {
 *   void          *p;
 *   unsigned long len;
 * } Descr;
 *
 * static short __tmp_ev1_code;
 * 
 * {  /* start a new symbol context */
 *
 *   Descr __delay_array[1] = { { ev1, 5 } };
 *
 *   __delay_event (1, &__delay_array, 7, NULL,
 *		    filename, linenumber);
 *
 * } /* end of symbol scope */
 */
#endif
void
build_delay_action (event, optpriority)
	tree event, optpriority;
{
  int had_errors = 0;
  tree to_loc = NULL_TREE;
  /* we discard the return value of __delay_event, cause in
     a normal DELAY action no selections have to be made */
  tree ev_got = null_pointer_node;
  
  /* check the event */
  if (event == NULL_TREE || TREE_CODE (event) == ERROR_MARK)
    had_errors = 1;
  else if (! CH_IS_EVENT_MODE (TREE_TYPE (event)) || ! CH_REFERABLE (event))
    {
      error ("DELAY action requires an event location.");
      had_errors = 1;
    }

  /* check the presence of priority */
  if (optpriority != NULL_TREE)
    {
      if (TREE_CODE (optpriority) == ERROR_MARK)
	return;
      if (TREE_CODE (TREE_TYPE (optpriority)) != INTEGER_TYPE)
	{
	  error ("PRIORITY in DELAY action must be of integer type.");
	  return;
	}
    }
  else
    {
      /* issue a warning in case of -Wall */
      if (extra_warnings)
	{
	  warning ("DELAY action without priority.");
	  warning (" PRIORITY defaulted to 0.");
	}
      optpriority = integer_zero_node;
    }
  if (had_errors)
    return;

  {
    tree descr_type;
    tree array_type_node;
    tree array_decl;
    tree descr_init;
    tree array_init;
    tree event_length = max_queue_size (TREE_TYPE (event));
    tree event_codes;
    tree filename = force_addr_of (get_chill_filename ());
    tree linenumber = get_chill_linenumber ();
    tree actuallist;

    to_loc = build_timeout_preface ();
    
    descr_type = TREE_TYPE (lookup_name (get_identifier ("__tmp_DESCR_type")));

    array_type_node =
        build_chill_array_type (descr_type,
	  tree_cons (NULL_TREE,
	    build_chill_range_type (NULL_TREE, integer_one_node,
				    integer_one_node),
		     NULL_TREE),
		       0, NULL_TREE);
    if (event_length == NULL_TREE)
      event_length = infinite_buffer_event_length_node;

    descr_init = 
      tree_cons (NULL_TREE, force_addr_of (event),
        tree_cons (NULL_TREE, event_length, NULL_TREE));
    array_init = 
      tree_cons (NULL_TREE,
		 build_nt (CONSTRUCTOR, NULL_TREE, descr_init),
		 NULL_TREE);
    array_decl = 
      decl_temp1 (get_unique_identifier ("event_codes_array"),
		  array_type_node, 0, 
		  build_nt (CONSTRUCTOR, NULL_TREE, array_init),
		  0, 0);

    event_codes =
      decl_temp1 (get_unique_identifier ("event_ptr"), 
		  ptr_type_node, 0, 
		  force_addr_of (array_decl),
		  0, 0);

    actuallist = 
      tree_cons (NULL_TREE, ev_got,
        tree_cons (NULL_TREE, integer_one_node,
	  tree_cons (NULL_TREE, event_codes,
            tree_cons (NULL_TREE, optpriority,
              tree_cons (NULL_TREE, to_loc,
	        tree_cons (NULL_TREE, null_pointer_node,
	          tree_cons (NULL_TREE, filename,
		    tree_cons (NULL_TREE, linenumber, NULL_TREE))))))));

		   
    build_timesupervised_call (
      build_chill_function_call (
	lookup_name (get_identifier ("__delay_event")),
	  actuallist), to_loc);
  }
}

void
expand_send_buffer (buffer, value, optpriority, optwith, optto)
     tree buffer, value, optpriority, optwith, optto;
{
  tree filename, linenumber;
  tree buffer_mode_decl = NULL_TREE;
  tree buffer_ptr, value_ptr;
  int  had_errors = 0;
  tree timeout_value, fcall;
  
  /* check buffer location */
  if (buffer == NULL_TREE || TREE_CODE (buffer) == ERROR_MARK)
    {
      buffer = NULL_TREE;
      had_errors = 1;
    }
  if (buffer != NULL_TREE)
    {
      if (! CH_IS_BUFFER_MODE (TREE_TYPE (buffer)) || ! CH_REFERABLE (buffer))
	{
	  error ("send buffer action requires a BUFFER location.");
	  had_errors = 1;
	}
      else
	buffer_mode_decl = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (buffer)));
    }

  /* check value and type */
  if (value == NULL_TREE || TREE_CODE (value) == ERROR_MARK)
    {
      had_errors = 1;
      value = NULL_TREE;
    }
  if (value != NULL_TREE)
    {
      if (TREE_CHAIN (value) != NULL_TREE)
	{
	  error ("there must be only 1 value for send buffer action.");
	  had_errors = 1;
	}
      else
	{
	  value = TREE_VALUE (value);
	  if (value == NULL_TREE || TREE_CODE (value) == ERROR_MARK)
	    {
	      had_errors = 1;
	      value = NULL_TREE;
	    }
	  if (value != NULL_TREE && buffer_mode_decl != NULL_TREE)
	    {
	      if (TREE_TYPE (buffer_mode_decl) != NULL_TREE &&
		  TREE_CODE (TREE_TYPE (buffer_mode_decl)) == ERROR_MARK)
		had_errors = 1;
	      else if (CH_COMPATIBLE (value, TREE_TYPE (buffer_mode_decl)))
		{
		  value = convert (TREE_TYPE (buffer_mode_decl), value);
		  if (value == NULL_TREE || TREE_CODE (value) == ERROR_MARK)
		    {
		      error ("convert failed for send buffer action.");
		      had_errors = 1;
		    }
		}
	      else
		{
		  error ("incompatible modes in send buffer action.");
		  had_errors = 1;
		}
	    }
	}
    }

  /* check the presence of priority */
  if (optpriority == NULL_TREE)
    {
      if (send_buffer_prio == NULL_TREE)
	{
	  /* issue a warning in case of -Wall */
	  if (extra_warnings)
	    {
	      warning ("Buffer sent without priority");
	      warning (" and no default priority was set.");
	      warning (" PRIORITY defaulted to 0.");
	    }
	  optpriority = integer_zero_node;
	}
      else
	optpriority = send_buffer_prio;
    }
  else if (TREE_CODE (optpriority) == ERROR_MARK)
    had_errors = 1;
  else if (TREE_CODE (TREE_TYPE (optpriority)) != INTEGER_TYPE)
    {
      error ("PRIORITY must be of integer type.");
      had_errors = 1;
    }

  if (optwith != NULL_TREE)
    {
      error ("WITH not allowed for send buffer action.");
      had_errors = 1;
    }
  if (optto != NULL_TREE)
    {
      error ("TO not allowed for send buffer action.");
      had_errors = 1;
    }
  if (had_errors)
    return;

  {
    tree descr_type;
    tree buffer_descr, buffer_init, buffer_length;
    tree val;

    /* process timeout */
    timeout_value = build_timeout_preface ();

    descr_type = lookup_name (get_identifier ("__tmp_DESCR_type"));

    /* build descr for buffer */
    buffer_length = max_queue_size (TREE_TYPE (buffer));
    if (buffer_length == NULL_TREE)
      buffer_length = infinite_buffer_event_length_node;
    buffer_init = build_nt (CONSTRUCTOR, NULL_TREE,
		    tree_cons (NULL_TREE, force_addr_of (buffer),
                      tree_cons (NULL_TREE, buffer_length, NULL_TREE)));
    buffer_descr = decl_temp1 (get_unique_identifier ("buffer_descr"),
			       TREE_TYPE (descr_type), 0, buffer_init,
			       0, 0);
    buffer_ptr = decl_temp1 (get_unique_identifier ("buffer_ptr"),
			     ptr_type_node, 0,
			     force_addr_of (buffer_descr),
			     0, 0);

    /* build descr for value */
    if (! CH_REFERABLE (value))
      val = decl_temp1 (get_identifier ("buffer_value"),
			TREE_TYPE (value), 0,
			value, 0, 0);
    else
      val = value;

    value_ptr = build_chill_descr (val);

  }

  /* get filename and linenumber */
  filename = force_addr_of (get_chill_filename ());
  linenumber = get_chill_linenumber ();
  
  /* Now, we can call the runtime */
  fcall = build_chill_function_call (
    lookup_name (get_identifier ("__send_buffer")),
      tree_cons (NULL_TREE, buffer_ptr,
	tree_cons (NULL_TREE, value_ptr,
	  tree_cons (NULL_TREE, optpriority,
            tree_cons (NULL_TREE, timeout_value,
              tree_cons (NULL_TREE, filename,
                tree_cons (NULL_TREE, linenumber, NULL_TREE)))))));
  build_timesupervised_call (fcall, timeout_value);
}
# if 0

void
process_buffer_decls (namelist, mode, optstatic)
  tree namelist, mode;
  int  optstatic;
{
  tree names;
  int quasi_flag = current_module->is_spec_module;

  if (pass < 2)
    return;

  for (names = namelist; names != NULL_TREE; names = TREE_CHAIN (names))
    { 
      tree name = TREE_VALUE (names);
      tree bufdecl = lookup_name (name);
      tree code_decl = 
	decl_tasking_code_variable (name, &buffer_code, quasi_flag);

      /* remember the code variable in the buffer decl */
      DECL_TASKING_CODE_DECL (bufdecl) = (struct lang_decl *)code_decl;

      add_taskstuff_to_list (code_decl, "_TT_Buffer", 
			     quasi_flag ? NULL_TREE : buffer_code,
			     bufdecl);
    }
}
#endif

/*
 * if no queue size was specified, QUEUESIZE is integer_zero_node.
 */
tree
build_buffer_type (element_type, queuesize)
     tree element_type, queuesize;
{
  tree type, field;
  if (element_type == NULL_TREE || TREE_CODE (element_type) == ERROR_MARK)
    return error_mark_node;
  if (queuesize != NULL_TREE && TREE_CODE (queuesize) == ERROR_MARK)
    return error_mark_node;

  type = make_node (RECORD_TYPE);
  field = build_decl (FIELD_DECL, get_identifier("__buffer_data"),
		      ptr_type_node);
  TYPE_FIELDS (type) = field;
  TREE_CHAIN (field)
    = build_lang_decl (TYPE_DECL, get_identifier ("__element_mode"),
		       element_type);
  field = TREE_CHAIN (field);
  if (queuesize)
    {
      tree size_field = build_decl (CONST_DECL, get_identifier("__queue_max"),
				    integer_type_node);
      DECL_INITIAL (size_field) = queuesize;
      TREE_CHAIN (field) = size_field;
    }
  CH_IS_BUFFER_MODE (type) = 1;
  CH_TYPE_NONVALUE_P (type) = 1;
  if (pass == 2)
    type = layout_chill_struct_type (type);
  return type;
}

#if 0
tree
build_buffer_descriptor (bufname, expr, optpriority)
     tree bufname, expr, optpriority;
{
  tree bufdecl;

  if (bufname == NULL_TREE
      || TREE_CODE (bufname) == ERROR_MARK)
    return error_mark_node;

  if (expr != NULL_TREE
      && TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;
#if 0
/* FIXME: is this what we really want to test? */
  bufdecl = lookup_name (bufname);
  if (TREE_CODE (bufdecl) != TYPE_DECL
      || ! CH_IS_BUFFER_MODE (TREE_TYPE (bufdecl)))
    {
      error ("SEND requires a BUFFER; `%s' is not a BUFFER name", 
	     bufname);
      return error_mark_node;
    }
#endif
  {
    /* build buffer/signal data structure */
    tree bufdataname = get_unique_identifier (IDENTIFIER_POINTER (bufname));
    tree dataptr;

    if (expr == NULL_TREE)
      dataptr = null_pointer_node;
    else
      {
	tree decl = 
	  decl_temp1 (bufdataname, TREE_TYPE (bufdecl), 0, 
		      expr, 0, 0);
	/* prevent granting of this variable */
	DECL_SOURCE_LINE (decl) = 0;

	dataptr = force_addr_of (decl);
      }
    
    /* build descriptor pointing to buffer data */
    {
      tree tasking_message_var = get_unique_identifier (IDENTIFIER_POINTER (bufname));
      tree data_len = (expr == NULL_TREE) ? integer_zero_node :
	                     size_in_bytes (TREE_TYPE (bufdecl));
      tree tasking_code = (tree)DECL_TASKING_CODE_DECL (bufdecl);
      tree tuple = build_nt (CONSTRUCTOR, NULL_TREE,
		     tree_cons (NULL_TREE, 
		       build1 (ADDR_EXPR, 
			       build_chill_pointer_type (chill_integer_type_node), 
			       tasking_code),
			   tree_cons (NULL_TREE, data_len,
			     tree_cons (NULL_TREE, dataptr, NULL_TREE))));
			      
      tree decl = decl_temp1 (tasking_message_var,
			      TREE_TYPE (tasking_message_type), 0,
			      tuple, 0, 0);
      mark_addressable (tasking_code);
      /* prevent granting of this variable */
      DECL_SOURCE_LINE (decl) = 0;

      tuple = force_addr_of (decl);
      return tuple;
    }
  }
}
#endif

#if 0
void
process_event_decls (namelist, mode, optstatic)
  tree namelist, mode;
  int  optstatic;
{
  tree names;
  int quasi_flag = current_module->is_spec_module;

  if (pass < 2)
    return;

  for (names = namelist; names != NULL_TREE; names = TREE_CHAIN (names))
    { 
      tree name = TREE_VALUE (names);
      tree eventdecl = lookup_name (name);
      tree code_decl = 
	decl_tasking_code_variable (name, &event_code, quasi_flag);

      /* remember the code variable in the event decl */
      DECL_TASKING_CODE_DECL (eventdecl) = (struct lang_decl *)code_decl;

      add_taskstuff_to_list (code_decl, "_TT_Event", 
			     quasi_flag ? NULL_TREE : event_code,
			     eventdecl);
    }
}
#endif

/* Return the buffer or event length of a buffer or event mode.
   (NULL_TREE means unlimited.) */

tree
max_queue_size (mode)
     tree mode;
{
  tree field = TYPE_FIELDS (mode);
  for ( ; field != NULL_TREE ; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == CONST_DECL)
	return DECL_INITIAL (field);
    }
  return NULL_TREE;
}

/* Return the buffer element mode of a buffer mode. */

tree
buffer_element_mode (bufmode)
     tree bufmode;
{
  tree field = TYPE_FIELDS (bufmode);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == TYPE_DECL)
	return TREE_TYPE (field);
    }
  return NULL_TREE;
}

/* invalidate buffer element mode in case we detect, that the
   elelment mode has the non-value property */

void
invalidate_buffer_element_mode (bufmode)
     tree bufmode;
{
  tree field = TYPE_FIELDS (bufmode);
  for ( ; field != NULL_TREE; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == TYPE_DECL)
	{
	  TREE_TYPE (field) = error_mark_node;
	  return;
	}
    }
}

/* For an EVENT or BUFFER mode TYPE, with a give maximum queue size QSIZE,
   perform various error checks.  Return a new queue size. */

tree
check_queue_size (qsize)
     tree qsize;
{
  if (qsize == NULL_TREE || TREE_CODE (qsize) == ERROR_MARK)
    return qsize;
  if (TREE_TYPE (qsize) == NULL_TREE
      || !CH_SIMILAR (TREE_TYPE (qsize), integer_type_node))
    {
      error ("non-integral max queue size for EVENT/BUFFER mode");
      return integer_one_node;
    }
  if (TREE_CODE (qsize) != INTEGER_CST)
    {
      error ("non-constant max queue size for EVENT/BUFFER mode");
      return integer_one_node;
    }
  if (compare_int_csts (pedantic ? LE_EXPR : LT_EXPR,
			qsize,
			integer_zero_node))
    {
      error ("max queue_size for EVENT/BUFFER is not positive");
      return integer_one_node;
    }
  return qsize;
}

/*
 * An EVENT type is modelled as a boolean type, which should
 * allocate the minimum amount of space.
 */
tree
build_event_type (queuesize)
     tree queuesize;
{
  tree type = make_node (RECORD_TYPE);
  tree field = build_decl (FIELD_DECL, get_identifier("__event_data"),
		      ptr_type_node);
  TYPE_FIELDS (type) = field;
  if (queuesize)
    {
      tree size_field = build_decl (CONST_DECL, get_identifier("__queue_max"),
				    integer_type_node);
      DECL_INITIAL (size_field) = queuesize;
      TREE_CHAIN (field) = size_field;
    }
  CH_IS_EVENT_MODE (type) = 1;
  CH_TYPE_NONVALUE_P (type) = 1;
  if (pass == 2)
    type = layout_chill_struct_type (type);
  return type;
}

/*
 * Initialize the various types of tasking data.
 */
void
tasking_init ()
{
  extern int  ignore_case;
  extern int  special_UC;
  extern tree chill_predefined_function_type;
  tree temp, ins_ftype_void;
  tree endlink = void_list_node;
  tree int_ftype_ptr_int_ptr_ptr_int_ptr_int_ptr_ptr_int;
  tree void_ftype_ptr;
  tree void_ftype_ptr_ins_int_int_ptr_ptr_int;
  tree int_ftype_ptr_ptr_int_ptr_ptr_int;
  tree void_ftype_int_int_int_ptr_ptr_ptr_int;
  tree int_ftype_ptr_int_ptr_int_ptr_ptr_ptr_int;
  tree int_ftype_ptr_int;

  /* type of tasking code variables */
  chill_taskingcode_type_node = short_unsigned_type_node;

  void_ftype_void =
       build_function_type (void_type_node,
	 tree_cons (NULL_TREE, void_type_node, NULL_TREE));

  build_instance_type ();
  ins_ftype_void
    = build_function_type (instance_type_node,
        tree_cons (NULL_TREE, void_type_node,
	  build_tree_list (NULL_TREE, void_type_node)));

  builtin_function ("__whoami", ins_ftype_void,
		    0, NOT_BUILT_IN, NULL_PTR);

  build_tasking_message_type ();
   
  temp = build_decl (TYPE_DECL,
	   get_identifier ("__tmp_TaskingStruct"),
	     build_tasking_struct ());
  pushdecl (temp);
  DECL_SOURCE_LINE (temp) = 0;

  /* any SIGNAL will be compatible with this one */
  generic_signal_type_node = copy_node (boolean_type_node);

  builtin_function ((ignore_case || ! special_UC) ? "copy_number" : "COPY_NUMBER",
		    chill_predefined_function_type,
		    BUILT_IN_COPY_NUMBER, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "gen_code" : "GEN_CODE",
		    chill_predefined_function_type,
		    BUILT_IN_GEN_CODE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "gen_inst" : "GEN_INST",
		    chill_predefined_function_type,
		    BUILT_IN_GEN_INST, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "gen_ptype" : "GEN_PTYPE",
		    chill_predefined_function_type,
		    BUILT_IN_GEN_PTYPE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "proc_type" : "PROC_TYPE",
		    chill_predefined_function_type,
		    BUILT_IN_PROC_TYPE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ((ignore_case || ! special_UC) ? "queue_length" : "QUEUE_LENGTH",
		    chill_predefined_function_type,
		    BUILT_IN_QUEUE_LENGTH, BUILT_IN_NORMAL, NULL_PTR);

  int_ftype_ptr_int_ptr_ptr_int_ptr_int_ptr_ptr_int
     = build_function_type (integer_type_node,
         tree_cons (NULL_TREE, ptr_type_node,
	   tree_cons (NULL_TREE, integer_type_node,
	     tree_cons (NULL_TREE, ptr_type_node,
	       tree_cons (NULL_TREE, ptr_type_node,
		 tree_cons (NULL_TREE, integer_type_node,
		   tree_cons (NULL_TREE, ptr_type_node,
		     tree_cons (NULL_TREE, integer_type_node,
                       tree_cons (NULL_TREE, ptr_type_node,
                         tree_cons (NULL_TREE, ptr_type_node,
                           tree_cons (NULL_TREE, integer_type_node,
			     endlink)))))))))));
  void_ftype_ptr
     = build_function_type (void_type_node,
           tree_cons (NULL_TREE, ptr_type_node, endlink));

  int_ftype_ptr_int_ptr_int_ptr_ptr_ptr_int
     = build_function_type (integer_type_node,
         tree_cons (NULL_TREE, ptr_type_node,
	   tree_cons (NULL_TREE, integer_type_node,
	     tree_cons (NULL_TREE, ptr_type_node,
               tree_cons (NULL_TREE, integer_type_node,
		 tree_cons (NULL_TREE, ptr_type_node,
		   tree_cons (NULL_TREE, ptr_type_node,
		     tree_cons (NULL_TREE, ptr_type_node,
		       tree_cons (NULL_TREE, integer_type_node,
		         endlink)))))))));

  void_ftype_ptr_ins_int_int_ptr_ptr_int
    = build_function_type (void_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, instance_type_node,
		  tree_cons (NULL_TREE, integer_type_node,
		      tree_cons (NULL_TREE, integer_type_node,
		        tree_cons (NULL_TREE, ptr_type_node,
			    tree_cons (NULL_TREE, ptr_type_node,
				tree_cons (NULL_TREE, integer_type_node,
		                    endlink))))))));
  int_ftype_ptr_ptr_int_ptr_ptr_int
    = build_function_type (integer_type_node,
	  tree_cons (NULL_TREE, ptr_type_node,
	    tree_cons (NULL_TREE, ptr_type_node,
	        tree_cons (NULL_TREE, integer_type_node,
		    tree_cons (NULL_TREE, ptr_type_node,
		        tree_cons (NULL_TREE, ptr_type_node,
		            tree_cons (NULL_TREE, integer_type_node,
		                endlink)))))));

  void_ftype_int_int_int_ptr_ptr_ptr_int
     = build_function_type (void_type_node,
	   tree_cons (NULL_TREE, integer_type_node,
	       tree_cons (NULL_TREE, integer_type_node,
		   tree_cons (NULL_TREE, integer_type_node,
		       tree_cons (NULL_TREE, ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
		               tree_cons (NULL_TREE, ptr_type_node,
				   tree_cons (NULL_TREE, integer_type_node,
			               endlink))))))));

  int_ftype_ptr_int
     = build_function_type (integer_type_node,
	   tree_cons (NULL_TREE, ptr_type_node,
               tree_cons (NULL_TREE, integer_type_node,
                   endlink)));

  builtin_function ("__delay_event", int_ftype_ptr_int_ptr_int_ptr_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__queue_length", int_ftype_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__register_tasking", void_ftype_ptr,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__send_signal", void_ftype_ptr_ins_int_int_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__send_buffer", int_ftype_ptr_ptr_int_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__start_process", void_ftype_int_int_int_ptr_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__stop_process", void_ftype_void, 0, NOT_BUILT_IN,
		    NULL_PTR);
  builtin_function ("__wait_buffer", int_ftype_ptr_int_ptr_ptr_int_ptr_int_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);
  builtin_function ("__wait_signal_timed", int_ftype_ptr_int_ptr_ptr_int_ptr_int_ptr_ptr_int,
		    0, NOT_BUILT_IN, NULL_PTR);

  infinite_buffer_event_length_node = build_int_2 (-1, 0);
  TREE_TYPE (infinite_buffer_event_length_node) = long_integer_type_node;
  TREE_UNSIGNED (infinite_buffer_event_length_node) = 1;
}
