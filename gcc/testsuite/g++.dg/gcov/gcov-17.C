/* { dg-options "--coverage -std=c++11" } */
/* { dg-do run { target native } } */

template <class T> class Foo
{
public:
  Foo () : b (1000) {}

  void inc () { b++; }

private:
  int b;
};

template class Foo<int>;
template class Foo<char>;

static void noret()
{
  __builtin_exit (0);
}

int
main (void)
{
  int i, total;
  Foo<int> counter;

  counter.inc ();
  counter.inc ();
  total = 0;

  for (i = 0; i < 10; i++)
    total += i;

  int v = total > 100 ? 1 : 2;

  if (total != 45)
    __builtin_printf ("Failure\n");
  else
    __builtin_printf ("Success\n");

  noret ();
  return 0;
}

/* { dg-final { run-gcov-pytest gcov-17.C "test-gcov-17.py" } } */
