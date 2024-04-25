/* Ensure jlt, jslt, jle and jsle instructions are not generated if
   -mjmpext is not enabled, and no 32-bit jump instructions are generated
   if -mjmp32 is not enabled.  */

/* { dg-do compile } */
/* { dg-options "-mno-jmpext -mno-jmp32 -masm=normal" } */

int foo (int a, int b)
{
  if (a == 1)
    b += 1;
  if (a != 3)
    b += 2;
  if (a > 5)
    b += 3;
  if (a >= 7)
    b += 4;
  if (a < 9)
    b += 5;
  if (a <= 10)
    b += 6;

  return a + b;
}

unsigned int bar (unsigned int a, unsigned int b)
{
  if (a == 1)
    b += 1;
  if (a != 3)
    b += 2;
  if (a > 5)
    b += 3;
  if (a >= 7)
    b += 4;
  if (a < 9)
    b += 5;
  if (a <= 10)
    b += 6;

  return a + b;
}

/* { dg-final { scan-assembler-times "jlt\t0" 0 } } */
/* { dg-final { scan-assembler-times "jslt\t0" 0 } } */
/* { dg-final { scan-assembler-times "jle\t0" 0 } } */
/* { dg-final { scan-assembler-times "jsle\t0" 0 } } */
/* { dg-final { scan-assembler-times "jeq32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jne32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jlt32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jgt32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jle32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jge32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jslt32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jsgt32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jsle32\t0" 0 } } */
/* { dg-final { scan-assembler-times "jsge32\t0" 0 } } */
