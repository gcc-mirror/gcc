/* { dg-do compile { target { musttail } } } */
/* { dg-options "-std=gnu++11" } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

extern void foo();

void f() noexcept
{
  __attribute__((musttail)) return foo(); /* { dg-error "call may throw exception that does not propagate" } */
}
