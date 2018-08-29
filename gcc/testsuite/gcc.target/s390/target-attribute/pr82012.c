/* Different target attributes in general prevent inlining.  However,
   we make an exception for soft-float callers if the callee doesn't
   actually use HW floating point.  This is currently required for
   compiling libitm.  */

/* { dg-options "-Wno-attributes" } */

double g = 1.0;

/* Inlining ok here.  foo1 doesn't use FP.  */

int __attribute__ ((always_inline)) foo1 (int a)
{
  return 0;
}

int __attribute__ ((target ("soft-float"))) test1 (int a)
{
  return foo1 (a);
}

/* Inlining ok here.  FP store doesn't need HW FP.  */

int __attribute__ ((always_inline)) foo2 (int a)
{
  g = 2.0;
  return 0;
}

int __attribute__ ((target ("soft-float"))) test2 (int a)
{
  return foo2 (a);
}

/* Inlining needs to be rejected.  foo3 performs HW FP operation.  */

int __attribute__ ((always_inline)) foo3 (int a) /* { dg-error "inlining failed in call to always_inline" } */
{
  g = (double) a / 2.0;
  return 0;
}

int __attribute__ ((target ("soft-float"))) test3 (int a)
{
  return foo3 (a);
}
