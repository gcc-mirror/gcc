/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options "-fpreprocessed -fdirectives-only -DNOT_SET" } */

/* Tests -fdirectives-only + -fpreprocessed. */

/* Check this is not defined. */
#ifdef NOT_SET
#error Command line macro not disabled.
#endif
