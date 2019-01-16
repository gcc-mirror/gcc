/* Verify that valid alignment on functions is accepted and results
   in the alignment expected for the target and that alignment of
   zero is ignored with a warning.
   { dg-do compile }
   { dg-options "-Wno-pedantic -ftrack-macro-expansion=0" }  */

#define ASSERT(expr)     _Static_assert (expr, #expr)
#define ALIGN(n)         __attribute__ ((aligned (n)))
#define alignof(expr)    __alignof__ (expr)
#define HAS_ALIGN(f, n)  __builtin_has_attribute (f, __aligned__ (n))

#define MINALIGN(N)   N
#define MAXALIGN      16

/* No alignment specified.  */
void f (void) { }

/* Empty alignment means maximum.  */
ALIGN () void f_ (void) { }

ALIGN (0) void f0 (void) { }    /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
ALIGN (1) void f1 (void) { }
ALIGN (2) void f2 (void) { }
ALIGN (4) void f4 (void) { }
ALIGN (8) void f8 (void) { }
ALIGN (16) void f16 (void) { }
ALIGN (32) void f32 (void) { }

ASSERT (alignof (f_) == MAXALIGN);
ASSERT (alignof (f0) == alignof (f));
ASSERT (alignof (f1) == MINALIGN (1));
ASSERT (alignof (f2) == MINALIGN (2));
ASSERT (alignof (f4) == MINALIGN (4));
ASSERT (alignof (f8) == MINALIGN (8));
ASSERT (alignof (f16) == MINALIGN (16));
ASSERT (alignof (f32) == MINALIGN (32));

ASSERT (!__builtin_has_attribute (f, aligned));
ASSERT (__builtin_has_attribute (f_, aligned));
ASSERT (!__builtin_has_attribute (f0, aligned));

ASSERT (!HAS_ALIGN (f_, MAXALIGN));

ASSERT (HAS_ALIGN (f1, 1));
ASSERT (!HAS_ALIGN (f1, 2));

ASSERT (!HAS_ALIGN (f2, 1));
ASSERT (HAS_ALIGN (f2, 2));
ASSERT (!HAS_ALIGN (f2, 4));

ASSERT (!HAS_ALIGN (f4, 2));
ASSERT (HAS_ALIGN (f4, 4));
ASSERT (!HAS_ALIGN (f4, 8));

ASSERT (!HAS_ALIGN (f8, 4));
ASSERT (HAS_ALIGN (f8, 8));
ASSERT (!HAS_ALIGN (f8, 16));

ASSERT (!HAS_ALIGN (f16, 8));
ASSERT (HAS_ALIGN (f16, 16));
ASSERT (!HAS_ALIGN (f16, 32));

ASSERT (!HAS_ALIGN (f32, 16));
ASSERT (HAS_ALIGN (f32, 32));
ASSERT (!HAS_ALIGN (f32, 64));
