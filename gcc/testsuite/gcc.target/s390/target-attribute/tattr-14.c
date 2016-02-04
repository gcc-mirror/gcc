/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mno-small-exec" } */

extern void foo(void);

#pragma GCC target("small-exec")
int p1(void)
{
  foo();
  return 1;
}
#pragma GCC reset_options

#pragma GCC target("no-small-exec")
int p0(void)
{
  foo();
  foo();
  return 2;
}
#pragma GCC reset_options

__attribute__ ((target("small-exec")))
int a1(void)
{
  foo();
  foo();
  foo();
  foo();
  return 4;
}

__attribute__ ((target("no-small-exec")))
int a0(void)
{
  foo();
  foo();
  foo();
  foo();
  foo();
  foo();
  foo();
  foo();
  return 8;
}

/* { dg-final { scan-assembler-times "\tbras\t" 5 } } */
