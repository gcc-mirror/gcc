/* Tree based linear points-to analysis
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree-alias-type.h"

alias_var
alias_var_new_with_aterm (tree decl, struct aterm_ *term)
{
  alias_var ret = ggc_alloc (sizeof (struct alias_var_aterm));
  ALIAS_VAR_KIND (ret) = ATERM_AVAR;
  ALIAS_VAR_DECL (ret) = decl;
  ALIAS_VAR_ATERM (ret) = term;
  return ret;
}
