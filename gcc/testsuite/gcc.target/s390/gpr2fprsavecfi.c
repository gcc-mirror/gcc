/* { dg-do compile } */
/* { dg-options "-O3 -march=z10 -mzarch -fdwarf2-cfi-asm" } */

char *gl[100];

long
foo ()
{
  long r = 0;
  char bla[100];
  int i;

  __builtin_memcpy (bla, gl, 100);

  for (i = 0; i < 100; i++)
    r += bla[i];

  return r;
}

/* { dg-final { scan-assembler-not "cfi_def_cfa_register" } } */
/* { dg-final { scan-assembler "cfi_register" } } */
/* { dg-final { scan-assembler "cfi_def_cfa_offset" } } */
