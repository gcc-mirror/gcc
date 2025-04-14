/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -march=armv8-a+rng" } */

__attribute__((target_version("default")))
__attribute__((optimize("O0")))
int foo ()
{ return 1; }

__attribute__((target_version("rng")))
int foo ();
__attribute__((target_version("flagm")))
int foo ();
__attribute__((target_version("rng+flagm")))
int foo ();

__attribute__((target_version("default")))
int bar()
{
  return foo ();
}

__attribute__((target_version("flagm")))
int bar();

/* { dg-final { scan-assembler-times "\n\tb\t_Z3foov\._Mrng\n" 1 } } */

