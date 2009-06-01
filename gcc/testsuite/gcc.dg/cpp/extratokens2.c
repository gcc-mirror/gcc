/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-Wno-endif-labels" } */

/* Tests that -Wno-endif-labels correctly disables the checks done by
   default (and tested in extratokens.c).  */

/* Source: Phil Edwards, 21 Mar 2002.  Copied from extratokens.c and
   modified.  */

#if 1 
#if 0
#else foo	/* { dg-bogus "extra tokens" "bad warning" } */
#endif /	/* { dg-bogus "extra tokens" "bad warning" } */
#endif

# 36 "file.c" 3

/* ... but in a system header, it's acceptable.  */
#ifdef KERNEL
#endif KERNEL  /* { dg-bogus "extra tokens" "bad warning" } */
