/* SUBREG liveness tracking classes for DF & IRA & LRA.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by Lehua Ding (lehua.ding@rivai.ai), RiVAI Technologies Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "subreg-live-range.h"

void
subregs_live::dump (FILE *file, const char *indent) const
{
  if (lives.empty ())
    {
      fprintf (file, "%sempty\n", indent);
      return;
    }
  fprintf (file, "%s", indent);
  for (auto &kv : lives)
    {
      const_sbitmap range = kv.second;
      if (bitmap_empty_p (range))
	continue;
      fprintf (file, "%d: ", kv.first);
      if (!bitmap_full_p (range))
	{
	  dump_bitmap_file (file, range);
	  fprintf (file, ",  ");
	}
      else
        fprintf (file, "full, ");
    }
  fprintf (file, "\n");
}

DEBUG_FUNCTION void
debug (const subregs_live &l)
{
  l.dump (stderr, "");
}
