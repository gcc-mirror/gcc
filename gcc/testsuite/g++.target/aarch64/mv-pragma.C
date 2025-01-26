/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-Wno-experimental-fmv-target" } */

#pragma GCC target ("+sve")

__attribute__((target_version("default")))
int foo ()
{
  return 1;
}

__attribute__((target_version("sve2")))
int foo ()
{
  return 2;
}

__attribute__((target_version("default")))
int bar ()
{
  return foo();
}

__attribute__((target_version("sha3")))
int bar ()
{
  return foo() + 5;
}

/* { dg-final { scan-assembler-times "\n\tbl\t_Z3foov\n" 2 } } */
