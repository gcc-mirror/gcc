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
  switch (int i = get ())
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (int i = 0)
    {
    case 0:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (auto i = get ())
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__typeof__(get ()) i = get ())
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (auto i = 0)
    {
    case 0:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch ([[maybe_unused]] int i = get ())
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__attribute__((unused)) int i = get ())
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (enum E e = X)
    {
    case X:
      foo (X);
      break;
    default:
      __builtin_abort ();
    }

  switch (constexpr int i = 42)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (register int i = 0);
  switch (static int i = 0);
  switch (_Atomic int i = 0);

  double i;
}

void
expr ()
{
  switch (int i = get (); i)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (auto i = get (); i)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__typeof__(get ()) i = get (); i)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (int (*f)(int) = foo; f (42))
    {
    case 42:
      foo (42);
      break;
    default:
      __builtin_abort ();
    }

  switch ([[maybe_unused]] int i = get (); i)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__attribute__((unused)) int i = get (); i)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (enum E e = X; e)
    {
    case X:
      foo (X);
      break;
    default:
      __builtin_abort ();
    }

  switch (constexpr int i = 42; i)
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (int arr[] = { 1, 2, 3}; arr[0])
    {
    case 1:
      foo (arr[0]);
      break;
    default:
      __builtin_abort ();
    }

  switch (register int i = 0; i);
  switch (static int i = 0; i);
  switch (_Atomic int i = 0; i);

  double i;
}

int
main ()
{
  simple ();
  expr ();
}
