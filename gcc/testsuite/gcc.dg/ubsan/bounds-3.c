/* PR sanitizer/70875 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

int
foo (int n, int k)
{
  struct S
  {
    int i[n];
    int value;
  } s[2];
  return s[k].value = 0;
}

int
main ()
{
  return foo (2, 2);
}

/* { dg-output "index 2 out of bounds for type 'S \\\[2\\\]'" } */
