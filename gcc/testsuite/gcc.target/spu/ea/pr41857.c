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

/* { dg-do compile } */

__ea char *strchr_ea (__ea const char *s, int c);
__ea char *foo (__ea char *s)
{
  __ea char *ret = s;
  int i;

  for (i = 0; i < 3; i++)
    ret = strchr_ea (ret, s[i]);
 
  return ret;
}
