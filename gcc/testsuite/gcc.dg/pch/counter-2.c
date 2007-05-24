/* Copyright (C) 2007 Free Software Foundation
   Contributed by Ollie Wild <aaw@google.com> */

/* { dg-options "-I. -Winvalid-pch" } */

#if __COUNTER__ != 0
#error __COUNTER__ != 0
#endif

#include "counter-2.h"/* { dg-error "`__COUNTER__' is invalid|No such file|they were invalid" } */

int main(void) 
{
  return __COUNTER__;
}
