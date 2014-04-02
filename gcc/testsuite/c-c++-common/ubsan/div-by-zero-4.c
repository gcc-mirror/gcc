/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -Wno-overflow" } */

#define INT_MIN (-__INT_MAX__ - 1)

int
main (void)
{
  /* This should not fail.  */
  return (unsigned int) INT_MIN / -1;
}
