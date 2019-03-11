/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options "-fdirectives-only -traditional" } */
/* { dg-error "'-fdirectives-only' is incompatible with '-traditional'\n" "'-traditional' check" { target *-*-* } 0 } */
