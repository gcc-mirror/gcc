/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -fomit-frame-pointer -mpreferred-stack-boundary=2" } */

int main ()
{
  volatile unsigned code = 0xc3;

  ((void (*)(void)) &code) ();
  return 0;
}

/* { dg-final { scan-assembler-not "call\[ \\t\]+\\*%esp" } } */
