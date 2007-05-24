/* Copyright (C) 2007 Free Software Foundation
   Contributed by Ollie Wild <aaw@google.com> */

#if __COUNTER__ != 0
#error __COUNTER__ != 0
#endif

#include "counter-3.h"

int main(void) 
{
  return __COUNTER__;
}
