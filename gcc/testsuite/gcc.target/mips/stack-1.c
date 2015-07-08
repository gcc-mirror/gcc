/* { dg-final { scan-assembler "\td?addiu\t(\\\$sp,)?\\\$sp,\[1-9\]" } } */
/* { dg-final { scan-assembler "\tlw\t" } } */
/* { dg-final { scan-assembler-not "\td?addiu\t(\\\$sp,)?\\\$sp,\[1-9\].*\tlw\t" } } */

/* Avoid use of SAVE, RESTORE and JRADDIUSP.  */
NOCOMPRESSION int foo (int y)
{
  volatile int a = y;
  volatile int *volatile b = &a;
  return *b;
}
