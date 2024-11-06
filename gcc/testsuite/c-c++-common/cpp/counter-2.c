/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options -fdirectives-only } */

/* Tests __COUNTER__ macro expansion is disabled inside directives with
   -fdirectives-only. */

#ifdef __COUNTER__  /* Macro not expanded. */
#endif

#if __COUNTER__ == 0  /* { dg-error "'__COUNTER__' expanded inside directive with '-fdirectives-only'" } */
#endif
