/* { dg-do run { target powerpc*-*-linux* powerpc-*-sysv* powerpc*-*-eabi* } } */
/* { dg-options {} } */

/* Test stack pointer alignment against variable alloca.  */
/* Inspired by PR libgcj/10610.  */
/* Origin: Franz Sirl <Franz.Sirl-kernel@lauterbach.com>.  */

extern void abort (void);
extern void exit (int);

register unsigned long sp __asm__ ("r1");

void g (int * val __attribute__ ((unused)))
{
  if (sp & 0xf)
    abort ();
}

void f (int val)
{
  int *val1 = __builtin_alloca (val);

  g (val1);
  return;
}

int main (void)
{
  int i;

  for (i = 1; i < 32; i++)
    f (i);
  
  exit (0);
}
