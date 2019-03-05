/* PR c/88172 - attribute aligned of zero silently accepted but ignored
   Verify that valid alignment on functions is accepted for all targets
   and that alignment of zero is ignored with a warning.
   { dg-do compile }
   { dg-options "-Wno-pedantic" }  */

#define ASSERT(expr)   _Static_assert (expr, #expr)
#define ALIGN(n)       __attribute__ ((aligned (n)))
#define alignof(expr)  __alignof__ (expr)

ALIGN (0) void f0 (void) { }    /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
ALIGN (1) void f1 (void) { }
ALIGN (2) void f2 (void) { }
ALIGN (3) void f3 (void) { }    /* { dg-error "requested alignment '3' is not a positive power of 2" } */
ALIGN (4) void f4 (void) { }

ASSERT (alignof (f0) > 0);
ASSERT (alignof (f1) >= 1);
ASSERT (alignof (f2) >= 2);
ASSERT (alignof (f3) >= 1);
ASSERT (alignof (f4) >= 4);
