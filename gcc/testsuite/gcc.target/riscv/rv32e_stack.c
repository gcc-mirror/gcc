/* { dg-do compile } */
/* { dg-options "-O0 -march=rv32e -mabi=ilp32e -fomit-frame-pointer" } */

int getInt();
void PrintInts(int);

int callPrintInts()
{
  int i = getInt();
  PrintInts(i);
  return i;
}

/* { dg-final { scan-assembler-not "addi	sp,sp,-16" } } */
