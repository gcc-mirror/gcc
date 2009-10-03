/* { dg-lto-do run }  */

int foo()
{
 double bar, baz = -__builtin_huge_val();
 return baz <= -bar;
}

int main()
{
  if (foo () != 1)
    return 1;
  return 0;
}
