/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* C++ silently ignores traditional! */
/* { dg-do preprocess { target c } } */
/* { dg-options "-fdirectives-only -traditional" } */
/* { dg-error "'-fdirectives-only' is incompatible with '-traditional'\n" "'-traditional' check" { target *-*-* } 0 } */
