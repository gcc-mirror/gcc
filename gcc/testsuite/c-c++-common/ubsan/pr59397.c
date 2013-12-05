/* { dg-do compile } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

typedef enum E { A = -1 } e;
int
foo (void)
{
  e e = A;
  return e + 1;
}
