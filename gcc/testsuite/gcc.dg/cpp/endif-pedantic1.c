/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-pedantic -Wno-endif-labels" } */

/* Tests combinations of -pedantic and -Wno-endif-labels; see extratokens2.c
   for more general tests.  */

/* Source: Phil Edwards, 25 Mar 2002.  Copied from extratokens2.c and
   modified.  */

#if 1 
#if 0
#else foo	/* { dg-bogus "extra tokens" "bad warning" } */
#endif /	/* { dg-bogus "extra tokens" "bad warning" } */
#endif

