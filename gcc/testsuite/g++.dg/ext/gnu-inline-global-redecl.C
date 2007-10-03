/* Test __attribute__((gnu_inline)).

   Check that we don't get out-of-line definitions for extern inline
   gnu_inline functions, regardless of redeclaration.

 */

/* { dg-do link } */
/* { dg-options "-O" } */ // such that static functions are optimized out

#include "gnu-inline-common.h"

decl(extern, fn)
gnuindef(fn, 0)
decl(extern, fn)

int main () {
  fn ();
}
