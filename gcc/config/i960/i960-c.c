/* Intel 80960 specific, C compiler specific functions.
   Copyright (C) 1992, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Contributed by Steven McGeady, Intel Corp.
   Additional Work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "cpplib.h"
#include "tree.h"
#include "c-pragma.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"

/* Handle pragmas for compatibility with Intel's compilers.  */

/* NOTE: ic960 R3.0 pragma align definition:

   #pragma align [(size)] | (identifier=size[,...])
   #pragma noalign [(identifier)[,...]]
     
   (all parens are optional)
     
   - size is [1,2,4,8,16]
   - noalign means size==1
   - applies only to component elements of a struct (and union?)
   - identifier applies to structure tag (only)
   - missing identifier means next struct
     
   - alignment rules for bitfields need more investigation.

   This implementation only handles the case of no identifiers.  */

void
i960_pr_align (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree number;
  enum cpp_ttype type;
  int align;

  type = c_lex (&number);
  if (type == CPP_OPEN_PAREN)
    type = c_lex (&number);
  if (type == CPP_NAME)
    {
      warning ("sorry, not implemented: #pragma align NAME=SIZE");
      return;
    }
  if (type != CPP_NUMBER)
    {
      warning ("malformed #pragma align - ignored");
      return;
    }

  align = TREE_INT_CST_LOW (number);
  switch (align)
    {
    case 0:
      /* Return to last alignment.  */
      align = i960_last_maxbitalignment / 8;
      /* Fall through.  */
    case 16:
    case 8:
    case 4:
    case 2:
    case 1:
      i960_last_maxbitalignment = i960_maxbitalignment;
      i960_maxbitalignment = align * 8;
      break;
      
    default:
      /* Silently ignore bad values.  */
      break;
    }
}

void
i960_pr_noalign (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  enum cpp_ttype type;
  tree number;

  type = c_lex (&number);
  if (type == CPP_OPEN_PAREN)
    type = c_lex (&number);
  if (type == CPP_NAME)
    {
      warning ("sorry, not implemented: #pragma noalign NAME");
      return;
    }

  i960_last_maxbitalignment = i960_maxbitalignment;
  i960_maxbitalignment = 8;
}
