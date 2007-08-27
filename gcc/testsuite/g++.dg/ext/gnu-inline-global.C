/* Test __attribute__((gnu_inline)).

   Check that __attribute__((gnu_inline)) has no effect, in the
   absence of extern and/or inline.

   Check that we don't get out-of-line definitions for extern inline
   gnu_inline functions, regardless of declarations or definitions.

   Check that such functions can be overridden by out-of-line
   definitions.

 */

/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler "func4" } } */
/* { dg-final { scan-assembler-not "func5" } } */

#include "gnu-inline-common.h"

#undef fn
#define fn pfx(func1) // must be emitted out-of-line
gnuindef(fn, 0)
def(, fn, 2)

#undef fn
#define fn pfx(func2) // must be emitted out-of-line
decl(extern, fn)
gnuindef(fn, 0)
def(, fn, 2)

#undef fn
#define fn pfx(func3) // must not be emitted
decl(extern, fn)
gnuindef(fn, 0)

#undef fn
#define fn pfx(func4) // must be emitted out-of-line
decl(extern, fn)
gnuindef(fn, 0)
def(, fn, 1)

#undef fn
#define fn pfx(func5) // must NOT be emitted, because it's static and unused
decl(static, fn)
gnuindef(fn, 0)
def(, fn, 1)
