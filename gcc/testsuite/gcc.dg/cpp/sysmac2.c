/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic -Wtraditional -ftrack-macro-expansion=0" } */

/* Tests diagnostics are suppressed for some macros defined in system
   headers.  */

/* Source: Neil Booth, 15 Jan 2001.  */

#include "syshdr.h"

#define uint 1U
#define fl 1.0f
#define ld 1.0L

int
main ()
{
  int u1 = uint;		/* { dg-warning "traditional C rejects" } */
  int u2 = sys_uint;		/* { dg-bogus "traditional C rejects" } */
  float f1 = fl;		/* { dg-warning "traditional C rejects" } */
  float f2 = sys_fl;		/* { dg-bogus "traditional C rejects" } */
  long double ld1 = ld;		/* { dg-warning "traditional C rejects" } */
  long double l2 = sys_ld;	/* { dg-bogus "traditional C rejects" } */

  return 0;
}
