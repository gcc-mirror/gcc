/* v850 specific, C compiler specific functions.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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
#include "cpplib.h"
#include "tree.h"
#include "c-pragma.h"
#include "c-lex.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"

#ifndef streq
#define streq(a,b) (strcmp (a, b) == 0)
#endif

static int  pop_data_area          PARAMS ((v850_data_area));
static int  push_data_area         PARAMS ((v850_data_area));
static void mark_current_function_as_interrupt PARAMS ((void));

/* Push a data area onto the stack.  */

static int
push_data_area (data_area)
     v850_data_area data_area;
{
  data_area_stack_element * elem;

  elem = (data_area_stack_element *) xmalloc (sizeof (* elem));

  if (elem == NULL)
    return 0;

  elem->prev      = data_area_stack;
  elem->data_area = data_area;

  data_area_stack = elem;

  return 1;
}

/* Remove a data area from the stack.  */

static int
pop_data_area (data_area)
     v850_data_area data_area;
{
  if (data_area_stack == NULL)
    warning ("#pragma GHS endXXXX found without previous startXXX");
  else if (data_area != data_area_stack->data_area)
    warning ("#pragma GHS endXXX does not match previous startXXX");
  else
    {
      data_area_stack_element * elem;

      elem = data_area_stack;
      data_area_stack = data_area_stack->prev;

      free (elem);

      return 1;
    }

  return 0;
}

/* Set the machine specific 'interrupt' attribute on the current function.  */

static void
mark_current_function_as_interrupt ()
{
  tree name;
  
  if (current_function_decl ==  NULL_TREE)
    {
      warning ("cannot set interrupt attribute: no current function");
      return;
    }

  name = get_identifier ("interrupt");

  if (name == NULL_TREE || TREE_CODE (name) != IDENTIFIER_NODE)
    {
      warning ("cannot set interrupt attribute: no such identifier");
      return;
    }
  
  decl_attributes (&current_function_decl,
		   tree_cons (name, NULL_TREE, NULL_TREE), 0);
}


/* Support for GHS pragmata.  */

void
ghs_pragma_section (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  int repeat;

  /* #pragma ghs section [name = alias [, name = alias [, ...]]] */
  do
    {
      tree x;
      enum cpp_ttype type;
      const char *sect, *alias;
      enum GHS_section_kind kind;
      
      type = c_lex (&x);
      
      if (type == CPP_EOF && !repeat)
	goto reset;
      else if (type == CPP_NAME)
	sect = IDENTIFIER_POINTER (x);
      else
	goto bad;
      repeat = 0;
      
      if (c_lex (&x) != CPP_EQ)
	goto bad;
      if (c_lex (&x) != CPP_NAME)
	goto bad;
      
      alias = IDENTIFIER_POINTER (x);
      
      type = c_lex (&x);
      if (type == CPP_COMMA)
	repeat = 1;
      else if (type != CPP_EOF)
	warning ("junk at end of #pragma ghs section");
      
      if      (streq (sect, "data"))    kind = GHS_SECTION_KIND_DATA;
      else if (streq (sect, "text"))    kind = GHS_SECTION_KIND_TEXT;
      else if (streq (sect, "rodata"))  kind = GHS_SECTION_KIND_RODATA;
      else if (streq (sect, "const"))   kind = GHS_SECTION_KIND_RODATA;
      else if (streq (sect, "rosdata")) kind = GHS_SECTION_KIND_ROSDATA;
      else if (streq (sect, "rozdata")) kind = GHS_SECTION_KIND_ROZDATA;
      else if (streq (sect, "sdata"))   kind = GHS_SECTION_KIND_SDATA;
      else if (streq (sect, "tdata"))   kind = GHS_SECTION_KIND_TDATA;
      else if (streq (sect, "zdata"))   kind = GHS_SECTION_KIND_ZDATA;
      /* According to GHS beta documentation, the following should not be
	 allowed!  */
      else if (streq (sect, "bss"))     kind = GHS_SECTION_KIND_BSS;
      else if (streq (sect, "zbss"))    kind = GHS_SECTION_KIND_ZDATA;
      else
	{
	  warning ("unrecognized section name \"%s\"", sect);
	  return;
	}
      
      if (streq (alias, "default"))
	GHS_current_section_names [kind] = NULL;
      else
	GHS_current_section_names [kind] =
	  build_string (strlen (alias) + 1, alias);
    }
  while (repeat);

  return;

 bad:
  warning ("malformed #pragma ghs section");
  return;

 reset:
  /* #pragma ghs section \n: Reset all section names back to their defaults.  */
  {
    int i;
    
    for (i = COUNT_OF_GHS_SECTION_KINDS; i--;)
      GHS_current_section_names [i] = NULL;
  }
}

void
ghs_pragma_interrupt (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs interrupt");
  
  mark_current_function_as_interrupt ();
}

void
ghs_pragma_starttda (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs starttda");
  
  push_data_area (DATA_AREA_TDA);
}

void
ghs_pragma_startsda (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs startsda");
  
  push_data_area (DATA_AREA_SDA);
}

void
ghs_pragma_startzda (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs startzda");
  
  push_data_area (DATA_AREA_ZDA);
}

void
ghs_pragma_endtda (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs endtda");
  
  pop_data_area (DATA_AREA_TDA);
}

void
ghs_pragma_endsda (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs endsda");
  
  pop_data_area (DATA_AREA_SDA);
}

void
ghs_pragma_endzda (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree x;
  
  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of #pragma ghs endzda");
  
  pop_data_area (DATA_AREA_ZDA);
}
