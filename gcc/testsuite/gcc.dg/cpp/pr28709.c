/* Copyright (C) 2006 Free Software Foundation, Inc.  */
/* PR preprocessor/28709 */

/* { dg-options "-ftrack-macro-expansion=0" }
   { dg-do compile } */

#define foo - ## >>
foo;
/* { dg-error "expected identifier.*'-'" "expected" { target *-*-* } 8 } */
/* { dg-error pasting "pasting" { target *-*-* } 8 } */
