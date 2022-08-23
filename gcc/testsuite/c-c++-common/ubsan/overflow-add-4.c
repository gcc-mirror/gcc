/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable -fsanitize-trap=undefined" } */
/* { dg-shouldfail "ubsan" } */

#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)

int
main (void)
{
  volatile int j = INT_MAX;
  volatile int i = 1;
  volatile int k = j + i;
  return 0;
}
