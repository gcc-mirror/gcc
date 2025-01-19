/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do run } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-options "-std=c2y -pedantic-errors" } */
/* Test C2Y if declarations.  Valid usages.  */

int get () { return 42; }
int foo (int i) { return i; }

enum E { X = 1, Y };

void
simple ()
{
  if (int i = get ())
    foo (i);
  else
    __builtin_abort ();

  if (int i = 0)
    __builtin_abort ();
  else
    foo (i);

  if (auto i = get ())
    foo (i);
  else
    __builtin_abort ();

  if (__typeof__(get ()) i = get ())
    foo (i);
  else
    __builtin_abort ();

  if (auto i = 0)
    __builtin_abort ();
  else
    foo (i);

  if (int (*f)(int) = foo)
    f (1);
  else
    __builtin_abort ();

  if ([[maybe_unused]] int i = get ())
    foo (i);
  else
    __builtin_abort ();

  if (__attribute__((unused)) int i = get ())
    foo (i);
  else
    __builtin_abort ();

  if (enum E e = X)
    foo (e);
  else
    __builtin_abort ();

  if (constexpr int i = 42)
    foo (i);
  else
    __builtin_abort ();

  if (int i = 1)
    if (int j = 2)
      if (int k = 3)
	foo (i + j + k);

  if (register int i = 0);
  if (static int i = 0);
  if (static int arr[3] = {});
  if (_Atomic int i = 0);

  if (int arr[] = { 1 })
    foo (arr[0]);
  else
    __builtin_abort ();

  double i;
}

void
expr ()
{
  if (int i = get (); i == 42)
    foo (i);
  else
    __builtin_abort ();

  if (int i = get (); i != 42)
    __builtin_abort ();
  else
    foo (i);

  if (auto i = get (); i == 42)
    foo (i);
  else
    __builtin_abort ();

  if (__typeof__(get ()) i = get (); i == 42)
    foo (i);
  else
    __builtin_abort ();

  if (auto i = get (); i != 42)
    __builtin_abort ();
  else
    foo (i);

  if (int (*f)(int) = foo; f (42))
    f (1);
  else
    __builtin_abort ();

  if ([[maybe_unused]] int i = get (); i == 42)
    foo (i);
  else
    __builtin_abort ();

  if (__attribute__((unused)) int i = get (); i == 42)
    foo (i);
  else
    __builtin_abort ();

  if (enum E e = X; e == X)
    foo (e);
  else
    __builtin_abort ();

  if (constexpr int i = 42; i == 42)
    foo (i);
  else
    __builtin_abort ();

  if (int i = 1; i)
    if (int j = 2; j)
      if (int k = 3; k)
	foo (i + j + k);

  if (int i = 2, j = get (); i + j > 0)
    foo (i + j);
  else
    __builtin_abort ();

  if (int i; i = 1)
    foo (i);
  else
    __builtin_abort ();

  if (int arr[] = { 1, 2, 3}; arr[0])
    foo (arr[0]);
  else
    __builtin_abort ();

  if (register int i = 0; i);
  if (static int i = 0; i);
  if (_Atomic int i = 0; i);

  double i;
}

int
main ()
{
  simple ();
  expr ();
}
