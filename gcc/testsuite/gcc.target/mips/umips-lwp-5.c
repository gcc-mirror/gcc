/* { dg-options "-mgp32 -fpeephole2 -mtune=m14k (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void MICROMIPS
foo (int *r4)
{
  int r5 = r4[512];
  int r6 = r4[513];
  r4[2] = r5 * r5;
  {
    register int r5asm asm ("$5") = r5;
    register int r6asm asm ("$6") = r6;
    asm ("#foo" : "=m" (r4[3]) : "d" (r5asm), "d" (r6asm));
  }
}

/* { dg-final { scan-assembler-not "\tlwp" } }*/
