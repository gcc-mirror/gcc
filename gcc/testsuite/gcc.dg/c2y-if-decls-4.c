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
  switch (int i = get ())  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (int i = 0)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 0:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (auto i = get ())  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__typeof__(get ()) i = get ())  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (auto i = 0)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 0:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch ([[maybe_unused]] int i = get ())  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__attribute__((unused)) int i = get ())  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (enum E e = X)  /* { dg-warning "if declarations before C2Y" } */
    {
    case X:
      foo (X);
      break;
    default:
      __builtin_abort ();
    }

  switch (constexpr int i = 42)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (register int i = 0); /* { dg-warning "if declarations before C2Y" } */
  switch (static int i = 0);  /* { dg-warning "if declarations before C2Y" } */
  switch (_Atomic int i = 0);  /* { dg-warning "if declarations before C2Y" } */

  double i;
}

void
expr ()
{
  switch (int i = get (); i)	/* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (auto i = get (); i)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__typeof__(get ()) i = get (); i)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (int (*f)(int) = foo; f (42))  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (42);
      break;
    default:
      __builtin_abort ();
    }

  switch ([[maybe_unused]] int i = get (); i)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (__attribute__((unused)) int i = get (); i)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (enum E e = X; e)  /* { dg-warning "if declarations before C2Y" } */
    {
    case X:
      foo (X);
      break;
    default:
      __builtin_abort ();
    }

  switch (constexpr int i = 42; i)  /* { dg-warning "if declarations before C2Y" } */
    {
    case 42:
      foo (i);
      break;
    default:
      __builtin_abort ();
    }

  switch (int arr[] = { 1, 2, 3}; arr[0])  /* { dg-warning "if declarations before C2Y" } */
    {
    case 1:
      foo (arr[0]);
      break;
    default:
      __builtin_abort ();
    }

  switch (register int i = 0; i); /* { dg-warning "if declarations before C2Y" } */
  switch (static int i = 0; i);  /* { dg-warning "if declarations before C2Y" } */
  switch (_Atomic int i = 0; i);  /* { dg-warning "if declarations before C2Y" } */

  double i;
}

int
main ()
{
  simple ();
  expr ();
}
