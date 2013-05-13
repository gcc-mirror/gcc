/* { dg-options "-mgp32 -fpeephole2 -mtune=m14k (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void MICROMIPS
f1 (int dummy, int *r5, int *other)
{
  int r4 = r5[0];
  int newr5 = r5[1];
  other[0] = r4 * r4;
  {
    register int r5asm asm ("$4") = r4;
    register int r4asm asm ("$5") = newr5;
    asm ("#foo" : "=m" (other[1]) : "d" (r4asm), "d" (r5asm));
  }
}

/* { dg-final { scan-assembler "\tlwp\t\\\$4,0\\(\\\$5\\)" } }*/
