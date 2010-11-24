/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mtune=generic -dp" } */

extern void fatal (void) __attribute__ ((__noreturn__));
extern void exit (int) __attribute__ ((__noreturn__));

void
fatal (void)
{
  exit (1);
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
