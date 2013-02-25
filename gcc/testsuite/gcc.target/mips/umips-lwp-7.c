/* { dg-options "-mgp32 -fpeephole2 -mtune=m14k (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void MICROMIPS
f1 (int *r4, int dummy, int *other)
{
  int r5 = r4[1];
  int newr4 = r4[0];
  other[0] = r5 * r5;
  {
    register int r5asm asm ("$5") = r5;
    register int r4asm asm ("$4") = newr4;
    asm ("#foo" : "=m" (other[1]) : "d" (r4asm), "d" (r5asm));
  }
}

void MICROMIPS
f2 (int *r4, int dummy, int *other)
{
  int newr4 = r4[0];
  int r5 = *(int *)(newr4 + 4);
  {
    register int r5asm asm ("$5") = r5;
    register int r4asm asm ("$4") = newr4;
    asm ("#foo" : "=m" (other[0]) : "d" (r4asm), "d" (r5asm));
  }
}

void MICROMIPS
f3 (int dummy, int *r5, int *other)
{
  int newr5 = r5[1];
  int r4 = *(int *)newr5;
  {
    register int r5asm asm ("$4") = r4;
    register int r4asm asm ("$5") = newr5;
    asm ("#foo" : "=m" (other[0]) : "d" (r4asm), "d" (r5asm));
  }
}

/* { dg-final { scan-assembler-not "\tlwp" } }*/
