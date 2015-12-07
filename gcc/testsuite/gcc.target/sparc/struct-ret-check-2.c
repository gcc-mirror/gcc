/* { dg-do run } */
/* { dg-options "-O -mstd-struct-return" } */
/* { dg-require-effective-target ilp32 } */

extern void abort (void);

struct S { int x, y, z; };

extern void bar (struct S *s) __attribute__ ((noinline, noclone));

void bar (struct S *s)
{
  s->x++;
}

struct S foo (void)
{
  struct S s = { 0, 2, 3 };
  bar (&s);
  return s;
}

int main (void)
{
  struct S s = foo ();
  if (s.x != 1)
    abort ();
  return 0;
}
