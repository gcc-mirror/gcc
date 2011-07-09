/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O1 -foptimize-sibling-calls" } */

void abort (void);

struct S
{
  void (__attribute__((__stdcall__)) *f) (struct S *);
  int i;
};

void __attribute__((__stdcall__))
foo (struct S *s)
{
  s->i++;
}

void __attribute__((__stdcall__))
bar (struct S *s)
{
  foo(s);
  s->f(s);
}

int main (void)
{
  struct S s = { foo, 0 };

  bar (&s);
  if (s.i != 2)
    abort ();

  return 0;
}

