/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Pragma buffers have a NULL "inc" member, which we would dereference
   when getting a file's date and time.

   Based on PR 7526.  14 Aug 2002.  */

#define GCC_PRAGMA(x) _Pragma (#x)
GCC_PRAGMA(GCC dependency "mi1c.h")
