/* { dg-do link } */
/* { dg-options "-O2 -fno-math-errno" } */

void link_error ();

double foo(void)
{
  return __builtin_pow(-__builtin_inf(), 3.5);
}

double bar(void)
{
  return __builtin_pow(-__builtin_inf(), 1e300);
}

int main()
{
  if (foo() != __builtin_inf())
    link_error ();
  if (bar() != __builtin_inf())
    link_error ();
  return 0;
}
