/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mtune=generic -dp" } */

extern void exit (int) __attribute__ ((__noreturn__));
extern void bar (void);

int
foo (int i)
{
  if (i == 0)
    {
      bar ();
      exit (1);
    }
  return 0;
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
