/* { dg-do run } */
/* { dg-options "-O2" } */

/* Test derived from PR 14643.  When a function has no addressable
   variables but 2 or more pointers have conflicting memory tags, they
   were not being processed by the type based alias analyzer,
   resulting in optimizations removing a non-redundant load.  */

extern void abort (void);

struct bar { int count;  int *arr;};

void foo (struct bar *b)
{
  b->count = 0;
  *(b->arr) = 2;
  if (b->count == 0)	/* b->count can't be assumed to be 0 here.  */
    abort ();
}

main ()
{
  struct bar x;
  x.arr = &x.count;
  foo (&x);
  return 0;
}
