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

/* Test default __EA32__/__EA64__ define.  */

/* { dg-do compile } */

#if !defined (__EA32__) && !defined (__EA64__)
#error both __EA32__ and __EA64__ undefined
#endif

#if defined (__EA32__) && defined (__EA64__)
#error both __EA32__ and __EA64__ defined
#endif

#ifdef __EA32__
int x [ sizeof (__ea char *) == 4 ? 1 : -1 ];
#endif

#ifdef __EA64__
int x [ sizeof (__ea char *) == 8 ? 1 : -1 ];
#endif

