/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-P" } */

/* Test that we don't stair-step output with -P.  Source: Neil Booth,
   18 Dec 2000.  */

int x = 1;

/* { dg-final { if ![file exists cmdlne-P.i] { return }                } }
   { dg-final { if { [grep cmdlne-P.i "^int x = 1;$"] != "" } { return } } }
   { dg-final { fail "cmdlne-P.c: stair-stepping with -P"              } } */
