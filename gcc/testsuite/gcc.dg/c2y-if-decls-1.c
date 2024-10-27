/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do run } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */
/* Test C2Y if declarations.  Valid usages.  */

int get () { return 42; }
int foo (int i) { return i; }

enum E { X = 1, Y };

void
simple ()
{
  if (int i = get ())  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (int i = 0)  /* { dg-warning "if declarations before C2Y" } */
    __builtin_abort ();
  else
    foo (i);

  if (auto i = get ())  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (__typeof__(get ()) i = get ())  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (auto i = 0)  /* { dg-warning "if declarations before C2Y" } */
    __builtin_abort ();
  else
    foo (i);

  if (int (*f)(int) = foo)  /* { dg-warning "if declarations before C2Y" } */
    f (1);
  else
    __builtin_abort ();

  if ([[maybe_unused]] int i = get ())  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (__attribute__((unused)) int i = get ())  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (enum E e = X)  /* { dg-warning "if declarations before C2Y" } */
    foo (e);
  else
    __builtin_abort ();

  if (constexpr int i = 42)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (int i = 1)  /* { dg-warning "if declarations before C2Y" } */
    if (int j = 2)  /* { dg-warning "if declarations before C2Y" } */
      if (int k = 3)  /* { dg-warning "if declarations before C2Y" } */
	foo (i + j + k);

  if (register int i = 0);  /* { dg-warning "if declarations before C2Y" } */
  if (static int i = 0);  /* { dg-warning "if declarations before C2Y" } */
  if (static int arr[3] = {});  /* { dg-warning "if declarations before C2Y" } */
  if (_Atomic int i = 0);  /* { dg-warning "if declarations before C2Y" } */

  if (int arr[] = { 1 })  /* { dg-warning "if declarations before C2Y" } */
    foo (arr[0]);
  else
    __builtin_abort ();

  double i;
}

void
expr ()
{
  if (int i = get (); i == 42)	/* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (int i = get (); i != 42)  /* { dg-warning "if declarations before C2Y" } */
    __builtin_abort ();
  else
    foo (i);

  if (auto i = get (); i == 42)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (__typeof__(get ()) i = get (); i == 42)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (auto i = get (); i != 42)  /* { dg-warning "if declarations before C2Y" } */
    __builtin_abort ();
  else
    foo (i);

  if (int (*f)(int) = foo; f (42))  /* { dg-warning "if declarations before C2Y" } */
    f (1);
  else
    __builtin_abort ();

  if ([[maybe_unused]] int i = get (); i == 42)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (__attribute__((unused)) int i = get (); i == 42)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (enum E e = X; e == X)  /* { dg-warning "if declarations before C2Y" } */
    foo (e);
  else
    __builtin_abort ();

  if (constexpr int i = 42; i == 42)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (int i = 1; i)  /* { dg-warning "if declarations before C2Y" } */
    if (int j = 2; j)  /* { dg-warning "if declarations before C2Y" } */
      if (int k = 3; k)  /* { dg-warning "if declarations before C2Y" } */
	foo (i + j + k);

  if (int i = 2, j = get (); i + j > 0)  /* { dg-warning "if declarations before C2Y" } */
    foo (i + j);
  else
    __builtin_abort ();

  if (int i; i = 1)  /* { dg-warning "if declarations before C2Y" } */
    foo (i);
  else
    __builtin_abort ();

  if (int arr[] = { 1, 2, 3}; arr[0])  /* { dg-warning "if declarations before C2Y" } */
    foo (arr[0]);
  else
    __builtin_abort ();

  if (register int i = 0; i);  /* { dg-warning "if declarations before C2Y" } */
  if (static int i = 0; i);  /* { dg-warning "if declarations before C2Y" } */
  if (_Atomic int i = 0; i);  /* { dg-warning "if declarations before C2Y" } */

  double i;
}

int
main ()
{
  simple ();
  expr ();
}
