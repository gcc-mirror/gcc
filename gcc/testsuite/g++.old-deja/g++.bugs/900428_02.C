// { dg-do assemble  }
// g++ 1.37.1 bug 900428_02

// g++ fails to issue either errors or warnings (even with -pedantic) for
// attempts to perform either pre or post increment or decrement operations
// on variables which have either pointer-to-void types or pointer-to-function
// types.

// cfront 2.0 passes this test.

// keywords: pointer arithmetic, increment, decrement

void *vp;
void (*fp) ();

void test ()
{
  vp++;		/* { dg-error "3:ISO C\\+\\+ forbids incrementing" } */
  ++vp;		/* { dg-error "5:ISO C\\+\\+ forbids incrementing" } */
  vp--;		/* { dg-error "3:ISO C\\+\\+ forbids decrementing" } */
  --vp;		/* { dg-error "5:ISO C\\+\\+ forbids decrementing" } */

  fp++;		/* { dg-error "3:ISO C\\+\\+ forbids incrementing" } */
  ++fp;		/* { dg-error "5:ISO C\\+\\+ forbids incrementing" } */
  fp--;		/* { dg-error "3:ISO C\\+\\+ forbids decrementing" } */
  --fp;		/* { dg-error "5:ISO C\\+\\+ forbids decrementing" } */
}
