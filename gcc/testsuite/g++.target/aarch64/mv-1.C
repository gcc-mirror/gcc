/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-Wno-experimental-fmv-target" } */

__attribute__((target_version("default")))
int foo ()
{
  return 1;
}

__attribute__((target_version("rng")))
int foo ()
{
  return 1;
}

__attribute__((target_version("flagm")))
int foo ()
{
  return 1;
}

__attribute__((target_version("rng+flagm")))
int foo ()
{
  return 1;
}

int bar()
{
  return foo ();
}

/* Check usage of the first two FMV features, in case of off-by-one errors.  */
/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mrng:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._MrngMflagm:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mflagm:\n" 1 } } */
