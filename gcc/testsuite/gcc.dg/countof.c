/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

#define assert(e)  ((e) ? (void) 0 : __builtin_abort ())

void
array (void)
{
  short a[7];

  static_assert (_Countof (a) == 7);
  static_assert (_Countof (unsigned [99]) == 99);
}

void
completed (void)
{
  int a[] = {1, 2, 3};

  static_assert (_Countof (a) == 3);
}

void
vla (void)
{
  unsigned n;

  n = 99;
  assert (_Countof (short [n - 10]) == 99 - 10);

  int v[n / 2];
  assert (_Countof (v) == 99 / 2);
}

void
member (void)
{
  struct {
    int a[8];
  } s;

  static_assert (_Countof (s.a) == 8);
}

void
vla_eval (void)
{
  int i;

  i = 7;
  assert (_Countof (struct {int x;}[i++]) == 7);
  assert (i == 7 + 1);

  int v[i];
  int (*p)[i];
  p = &v;
  assert (_Countof (*p++) == i);
  assert (p - 1 == &v);
}

void
array_noeval (void)
{
  long a[5];
  long (*p)[_Countof (a)];

  p = &a;
  static_assert (_Countof (*p++) == 5);
  assert (p == &a);
}

void
matrix_fixed (void)
{
  int i;

  static_assert (_Countof (int [7][4]) == 7);
  i = 3;
  static_assert (_Countof (int [7][i]) == 7);
}

void
matrix_vla (void)
{
  int i, j;

  i = 7;
  assert (_Countof (int [i++][4]) == 7);
  assert (i == 7 + 1);

  i = 9;
  j = 3;
  assert (_Countof (int [i++][j]) == 9);
  assert (i == 9 + 1);
}

void
no_parens(void)
{
  int n = 3;
  int a[7];
  int v[n];

  static_assert (_Countof a == 7); 
  assert (_Countof v == 3); 
}

int
main (void)
{
  array ();
  completed ();
  vla ();
  member ();
  vla_eval ();
  array_noeval ();
  matrix_fixed ();
  matrix_vla ();
  no_parens ();
}
