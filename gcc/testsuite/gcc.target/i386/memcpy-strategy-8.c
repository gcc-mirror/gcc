/* { dg-do compile } */
/* { dg-options "-O2 -march=tigerlake" } */
/* { dg-final { scan-assembler-not "jmp\tmemcpy" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "call\tmemcpy" { target ia32 } } } */
/* { dg-final { scan-assembler-not "rep movsb" } } */

typedef unsigned char e_u8;

#define MAXBC 8

void MixColumn(e_u8 a[4][MAXBC], e_u8 BC)
{
  e_u8 b[4][MAXBC];
  int i, j;

  for(i = 0; i < 4; i++)
    for(j = 0; j < BC; j++) a[i][j] = b[i][j];
}
