// { dg-do run }
// { dg-require-weak "" }
// { dg-options "-O2 -fno-common" }

// Copyright 2005 Free Software Foundation, Inc.
// Contributed by Alexandre Oliva <aoliva@redhat.com>

// PR middle-end/24295

// The unit-at-a-time call graph code used to fail to emit variables
// without external linkage that were only used indirectly, through
// aliases.  We might then get linker failures because the static
// variable was not defined, or run-time errors because the weak alias
// ended up pointing somewhere random.

#include <stdlib.h>

static unsigned long lv1 = 0xdeadbeefUL;
#pragma weak Av1a = lv1
extern unsigned long Av1a;

static unsigned long lf1(void) { return 0x510bea7UL; }
#pragma weak Af1a = lf1
extern unsigned long Af1a(void);

int main (void) {
  if (! &Av1a
      || ! &Af1a
      || Av1a != 0xdeadbeefUL
      || Af1a() != 0x510bea7UL)
    abort ();
  exit (0);
}
