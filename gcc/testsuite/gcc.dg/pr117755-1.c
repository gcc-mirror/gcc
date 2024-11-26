/* Test ICE with atomic compound assignment with invalid operands in nested
   function (bug 117755).  */
/* { dg-do compile } */
/* { dg-options "" } */

_Atomic struct S { char c; } as;

void
f (void)
{
  void g (void)
  {
    as += 1; /* { dg-error "invalid operands to binary" } */
  }
}
