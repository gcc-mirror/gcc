/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wlarger-than-32768" } */

/* -Wlarger-than with functions returning void used to segfault.
   Source: PR 602, testsuite-ized by Neil Booth 21 Jan 2000.  */

static void foo (void) {}

