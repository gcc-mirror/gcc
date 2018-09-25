// PR c++/87152
// { dg-do run }
// { dg-options "-std=c++2a" }

template<typename>
int foo ()
{
  int a[] = { 1, 2, 3, 4, 5 };
  int j = 0;
  for (int i = 0; auto x : a)
    j += i++;

  return j;
}

int
main ()
{
  int j = foo<int>();
  if (j != 10)
    __builtin_abort ();
}
