/* Copyright (C) 2006 Free Software Foundation, Inc.  */
/* PR preprocessor/28709 */

/* { dg-do compile } */
#define foo - ## >>
foo;
/* { dg-error "expected identifier.*'-'" "" { target *-*-* } 6 } */
/* { dg-error pasting "" { target *-*-* } 6 } */
