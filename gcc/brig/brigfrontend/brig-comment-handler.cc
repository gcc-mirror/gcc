/* brig-comment-handler.cc -- brig comment directive handling
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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

#include "brig-code-entry-handler.h"

extern int gccbrig_verbose;

size_t
brig_directive_comment_handler::operator () (const BrigBase *base)
{
  const BrigDirectiveComment *brig_comment
    = (const BrigDirectiveComment *) base;

  if (gccbrig_verbose)
    {
      std::string cmnt = m_parent.get_string (brig_comment->name);
      fprintf (stderr, "brig: Comment: '%s'\n", cmnt.c_str());
    }
  return base->byteCount;
}
