/* A "buffer" utility type.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/* Written by Per Bothner <bothner@cygnus.com>, July 1998. */

#include "config.h"
#include "system.h"
#include "buffer.h"

/* Grow BUFP so there is room for at least SIZE more bytes. */

void
buffer_grow (bufp, size)
     struct buffer *bufp;
     int size;
{
  if (bufp->limit - bufp->ptr >= size)
    return;
  if (bufp->data == 0)
    {
      if (size < 120)
	size = 120;
      bufp->data = xmalloc (size);
      bufp->ptr = bufp->data;
    }
  else
    {
      int index = bufp->ptr - bufp->data;
      size += 2 * (bufp->limit - bufp->data);
      bufp->data = xrealloc (bufp->data, size);
      bufp->ptr = bufp->data + index;
    }
  bufp->limit = bufp->data + size;
}
