// PR c++/79232
// { dg-do run }
// { dg-options "-fstrong-eval-order" }

int last = 0;

int
foo (int i)
{
  if (i != last + 1)
    __builtin_abort ();
  last = i;
  return i;
}

char a, b;
int c;

char &
bar (int i, int j)
{
  foo (i);
  return j ? a : b;
}

int
main ()
{
  (foo (2) ? bar (3, 0) : bar (3, 1)) = foo (1);
  if (last != 3)
    __builtin_abort ();
  last = 0;
  (foo (2), foo (3) ? bar (4, 0) : bar (4, 1)) = foo (1);
  if (last != 4)
    __builtin_abort ();
  last = 0;
  (foo (2), (foo (3) ? bar (4, 0) : bar (4, 1))) = foo (1);
  if (last != 4)
    __builtin_abort ();
  last = 0;
  (foo (2), foo (3), foo (4) ? bar (5, 0) : bar (5, 1)) = foo (1);
  if (last != 5)
    __builtin_abort ();
  last = 0;
  (foo (2), (foo (3), (foo (4) ? bar (5, 0) : bar (5, 1)))) = foo (1);
  if (last != 5)
    __builtin_abort ();
  last = 0;
  --c = foo (1);
  if (c != 1)
    __builtin_abort ();
  last = 0;
  (foo (2), --c) = foo (1);
  if (last != 2 || c != 1)
    __builtin_abort ();
  last = 0;
  (foo (2), foo (3), --c) = foo (1);
  if (last != 3 || c != 1)
    __builtin_abort ();
  last = 0;
  (foo (2), (foo (3), --c)) = foo (1);
  if (last != 3 || c != 1)
    __builtin_abort ();
  last = 0;
  bar (2, 0) = foo (1);
  if (last != 2)
    __builtin_abort ();
  last = 0;
  (foo (2), bar (3, 0)) = foo (1);
  if (last != 3)
    __builtin_abort ();
  last = 0;
  (foo (2), foo (3), bar (4, 0)) = foo (1);
  if (last != 4)
    __builtin_abort ();
  last = 0;
  (foo (2), (foo (3), bar (4, 0))) = foo (1);
  if (last != 4)
    __builtin_abort ();
}
