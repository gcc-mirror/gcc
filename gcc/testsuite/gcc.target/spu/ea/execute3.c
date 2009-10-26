/* Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* { dg-do run } */

#include <stdlib.h>

int
main (void)
{
  __ea char *p = (__ea char *)"abc";

  if (*p++ != 'a')
    abort ();

  if (*p++ != 'b')
    abort ();

  if (*p++ != 'c')
    abort ();

  if (*p++ != '\0')
    abort ();

  return 0;
}
