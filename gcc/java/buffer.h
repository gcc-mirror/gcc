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

/* A simple data structure for an expandable buffer. */

struct buffer
{
  /* The start of the actual data buffer. */
  unsigned char *data;

  /* Where to write next in the buffer. */
  unsigned char *ptr;

  /* The end of the allocated data buffer. */
  unsigned char *limit;
};

#define NULL_BUFFER { (void*) 0, (void*) 0, (void*) 0 }

#define BUFFER_INIT(BUFP) \
  ((BUFP)->data = NULL, (BUFP)->ptr = NULL, (BUFP)->limit = NULL)

#define BUFFER_LENGTH(BUFP) ((BUFP)->ptr - (BUFP)->data)

#define BUFFER_RESET(BUFP) ((BUFP)->ptr = (BUFP)->data)

extern void buffer_grow PROTO ((struct buffer*, int));
