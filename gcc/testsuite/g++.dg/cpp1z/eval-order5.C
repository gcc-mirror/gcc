// PR c++/91974
// { dg-do run }
// { dg-options "-fstrong-eval-order" }

extern "C" void abort ();

bool ok = false;

void
foo (int x)
{
  if (x != 0)
    abort ();
  ok = true;
}

void
bar (int)
{
  abort ();
}

int
main ()
{
  typedef void (*T) (int);
  T fn = foo;
  fn ((fn = bar, 0));
  if (fn != bar || !ok)
    abort ();
}
