/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mtune=generic -dp" } */

extern void exit (int) __attribute__ ((__noreturn__));

int
foo (int i)
{
  if (i == 0)
    exit (1);
  return 0;
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
