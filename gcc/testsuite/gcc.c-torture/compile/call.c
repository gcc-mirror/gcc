int foo () {}

main (a, b)
{
  foo (foo (a, b), foo (b, a));
  return 0;
}
