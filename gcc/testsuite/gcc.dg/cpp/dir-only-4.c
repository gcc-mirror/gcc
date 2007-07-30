/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options "-fdirectives-only -Wunused-macros" } */
/* { dg-error "-fdirectives-only is incompatible with -Wunused_macros\n" "-Wunused-macros check" { target *-*-* } 0 } */
