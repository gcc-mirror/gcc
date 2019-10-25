/* { dg-do run } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

struct S { _Bool x; };

void
foo (struct S *s)
{
  __builtin_memset (s, 1, sizeof (struct S));
  s->x = 1;
}

int
main ()
{
  struct S s;
  foo (&s);
  char c;
  __builtin_memcpy (&c, &s.x, 1);
  if (c != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "Deleted redundant store" "fre1" } } */
