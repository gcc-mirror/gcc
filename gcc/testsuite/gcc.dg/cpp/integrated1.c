/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-g -include mi1c.h" } */

/* Tests that -g -include doesn't segfault.  This used to happen
   because cpp_start_read would be called before initialising debug
   output for the integrated front ends.

   Contributed by Neil Booth 15 Nov 2000.  */

