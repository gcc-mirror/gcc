/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

#define INT_MIN (-__INT_MAX__ - 1)

int
main ()
{
  int x = INT_MIN;
  int y = 0;
  int z;
  __asm__ volatile ("" : "+g" (y));
  __asm__ volatile ("" : "+g" (x));
  z = y - (-x);
  __asm__ volatile ("" : "+g" (z));
}

/* { dg-output "negation of -2147483648 cannot be represented in type 'int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0 - -2147483648 cannot be represented in type 'int'" } */
