/* { dg-do compile } */
/* { dg-options "-fpic" } */

extern int var, arr[2][2];

void
test (void)
{
  __asm__ ("@ %0" : : "i"(&var));
  __asm__ ("@ %0 %1 %2" : : "s"(&var), "s"(&arr[1][1]), "s"(test));
}

/* { dg-final { scan-assembler "@ var arr\\+12 test" } } */
/* { dg-final { scan-assembler "@ var" } } */
