/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32imafc -mabi=ilp32f" } */
char my_getchar();
float getf();

float foo()
{
  char volatile array[3120];
  float volatile farray[3120];
  float sum = 0;
  float f1 = getf();
  float f2 = getf();
  float f3 = getf();
  float f4 = getf();

  for (int i = 0; i < 3120; i++)
  {
    array[i] = my_getchar();
    farray[i] = my_getchar() * 1.2;
    sum += array[i] + farray[i] + f1 + f2 + f3 + f4;
  }

  return sum;
}

/* { dg-final { scan-assembler-not {,-[0-9]+\(sp\)} } } */
