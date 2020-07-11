/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options "-fdirectives-only -fpreprocessed" } */

/* Tests __COUNTER__ macro expansion is enabled outside directives with
   -fdirectives-only. */

int zero = __COUNTER__;
