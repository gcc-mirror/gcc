/* Reverse order of definitions obtained from bytecode definition file.
   Copyright (C) 1993 Free Software Foundation, Inc.

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


#include "hconfig.h"
#include "bi-defs.h"

void
reverse()
{
  struct def *dp, *d, *dn;
  struct variation *vp, *v, *vn;

  dp = defs;
  if (dp)
    {
      vp = dp->variations;
      if (vp)
	{
	  for (v = vp->next, vp->next = 0; v; vp = v, v = vn)
	    {
	      vn = v->next;
	      v->next = vp;
	    }
	  dp->variations = vp;
	}
      for (d = dp->next, dp->next = 0; d; dp = d, d = dn)
	{
	  vp = d->variations;
	  if (vp)
	    {
	      for (v = vp->next, vp->next = 0; v; vp = v, v = vn)
		{
		  vn = v->next;
		  v->next = vp;
		}
	      d->variations = vp;
	    }
	  dn = d->next;
	  d->next = dp;
	}
      defs = dp;
    }
}
