/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Source: Neil Booth, 2 Oct 2001.

   Tests that we clear the disabled flag that is set by the
   macro-defined-to-itself optimization (the optimization might not be
   worth it).  */

#define foo foo
#undef foo
#define foo 1
#if !foo
#error foo still disabled! 
#endif
