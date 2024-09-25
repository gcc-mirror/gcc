/* { dg-do compile { target { musttail } } } */
/* { dg-options "-std=gnu++11" } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

extern void foo();

void f() noexcept
{
  [[gnu::musttail]] return foo(); /* { dg-error "call may throw exception that does not propagate" } */
}
