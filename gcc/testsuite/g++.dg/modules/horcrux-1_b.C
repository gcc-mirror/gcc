// { dg-additional-options -fmodules-ts }

module foo;

int main ()
{
  __is_constructible_impl<bool> x;
  return 0;
}
