/* Copyright (C) 2007 Free Software Foundation
   Contributed by Ollie Wild <aaw@google.com> */

/* { dg-options "-I. -Winvalid-pch" } */

#if __COUNTER__ != 0
#error __COUNTER__ != 0
#endif

#include "counter-2.h" /* { dg-warning "not used because `__COUNTER__' is invalid" } */
/* { dg-error "counter-2.h: No such file or directory" "" { target *-*-* } 10 } */
/* { dg-error "one or more PCH files were found, but they were invalid" "" { target *-*-* } 10 } */

int main(void) 
{
  return __COUNTER__;
}
