/* Implement Input/Output runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser, et al

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <setjmp.h>

/* define names of IO-exceptions */

char * __IO_exception_names[] =
{
  "UNUSED",
  "notassociated",
  "associatefail",
  "createfail",
  "deletefail",
  "modifyfail",
  "connectfail",
  "notconnected",
  "empty",
  "rangefail",
  "spacefail",
  "readfail",
  "writefail",
  "textfail",
};

jmp_buf __io_exception;

jmp_buf __rw_exception;
