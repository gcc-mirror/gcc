/* EH stuff
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


/* This file contains the structures required for the language
   independant exception handling model. Both the static compiler and
   the runtime library share this file. */

/* The runtime flag flag_new_exceptions is used to determine whether the 
   compiler supports the new runtime typechecking mechanism or not. Under
   the new model, runtime info is contained in the exception table, and
   the __throw() library routine determines which handler to call based
   on the results of a call to a matching function provided by the expcetion
   thrower.  Otherwise the old scheme of calling any handler which matches
   an exception range is used, and the handler is responsible for all
   checking of runtime conditions. If the handler wasn't suppose to
   get the exception, it performs a re-throw. */


/* The handler_label field MUST be the first field in this structure. The 
   __throw()  library routine expects uses __eh_stub() from except.c, which
   simply dereferences the context pointer to get the handler.
   The routine get_dynamic_handler_chain() also has a dependancy on
   the location of 'dynamic_handler_chain'. If its location is changed, 
   that routine must be modified as well. */

struct eh_context
{
  void *handler_label;
  void **dynamic_handler_chain;
  /* This is language dependent part of the eh context. */
  void *info;
  /* This is used to remember where we threw for re-throws */
  void *table_index;  /* address of exception table entry to rethrow from */
};

#ifndef EH_TABLE_LOOKUP

typedef struct old_exception_table 
{
  void *start_region;
  void *end_region;
  void *exception_handler;
} old_exception_table;

typedef struct exception_table 
{
  void *start_region;
  void *end_region;
  void *exception_handler;
  void *match_info;              /* runtime type info */
} exception_table;


/* The language identifying portion of an exception table */

typedef struct exception_lang_info 
{
  short language;
  short version;  
} exception_lang_info;

/* This value in the first field of the exception descriptor 
   identifies the descriptor as the new model format. This value would never
   be present in this location under the old model */

#define NEW_EH_RUNTIME  ((void *) -2)

/* Each function has an exception_descriptor which contains the
   language info, and a table of exception ranges and handlers */

typedef struct exception_descriptor 
{
  void *runtime_id_field;    
  exception_lang_info lang;
  exception_table table[1];
} exception_descriptor;

struct __eh_info; /* forward declaration */

/* A pointer to a matching function is initialized at runtime by the 
   specific language if run-time exceptions are supported. 
   The function takes 3 parameters
    1 - runtime exception that has been thrown info. (__eh_info *)
    2 - Match info pointer from the region being considered (void *)
    3 - exception table region is in (exception descriptor *)
*/

typedef void * (*__eh_matcher)	PARAMS ((struct __eh_info *, void *,
					 struct exception_descriptor *));

/* This value is to be checked as a 'match all' case in the runtime field. */

#define CATCH_ALL_TYPE   ((void *) -1)

/* This is the runtime exception information. This forms the minimum required
   information for an exception info pointer in an eh_context structure. */


typedef struct __eh_info 
{
  __eh_matcher match_function;
  short language;
  short version;
} __eh_info;

/* Convienient language codes for ID the originating language. Similar
   to the codes in dwarf2.h. */

enum exception_source_language
  {
    EH_LANG_C89 = 0x0001,
    EH_LANG_C = 0x0002,
    EH_LANG_Ada83 = 0x0003,
    EH_LANG_C_plus_plus = 0x0004,
    EH_LANG_Cobol74 = 0x0005,
    EH_LANG_Cobol85 = 0x0006,
    EH_LANG_Fortran77 = 0x0007,
    EH_LANG_Fortran90 = 0x0008,
    EH_LANG_Pascal83 = 0x0009,
    EH_LANG_Modula2 = 0x000a,
    EH_LANG_Java = 0x000b,
    EH_LANG_Mips_Assembler = 0x8001
  };

#endif  /* EH_TABLE_LOOKUP */


