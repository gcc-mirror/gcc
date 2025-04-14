/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -march=armv8-a+rng+flagm" } */

__attribute__((target_version("default")))
__attribute__((optimize("O0")))
int foo ()
{
  return 1;
}

__attribute__((target_version("rng")))
__attribute__((optimize("O0")))
int foo ()
{
  return 2;
}

__attribute__((target_version("flagm")))
__attribute__((optimize("O0")))
int foo ()
{
  return 3;
}

__attribute__((target_version("rng+flagm")))
__attribute__((optimize("O0")))
int foo ()
{
  return 4;
}

int bar()
{
  return foo ();
}

/* Can optimize to highest priority function */
/* { dg-final { scan-assembler-times "\n\tb\t_Z3foov\._MrngMflagm\n" 1 } } */

