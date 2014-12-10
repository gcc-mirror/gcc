/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom -fno-pic" } */
/* { dg-final { scan-assembler-not "nop" } } */
/* { dg-final { scan-assembler-not "rep" } } */

int s[8] = {1, 2, 3, 4, 5, 6, 7, 8};
int d[8] = {11, 22, 33, 44, 55, 66, 77, 88};

void
foo ()
{
  int i;
  for (i = 0; i < 8; i++)
    d[i] = s[i] + 0x1000;
}
