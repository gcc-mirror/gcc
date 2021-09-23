/* { dg-do compile } */
/* { dg-options "-O2 -march=tigerlake" } */
/* { dg-final { scan-assembler-not "jmp\tmemset" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "call\tmemset" { target ia32 } } } */
/* { dg-final { scan-assembler-not "rep stosb" } } */

typedef unsigned char e_u8;

#define MAXBC 8

void MixColumn(e_u8 a[4][MAXBC], e_u8 BC)
{
  int i, j;

  for(i = 0; i < 4; i++)
    for(j = 0; j < BC; j++) a[i][j] = 1;
}
