/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O3 -fkeep-inline-functions -fsel-sched-pipelining -fselective-scheduling2 -funroll-loops" } */

struct S
{
  unsigned i, j;
};

static inline void
bar (struct S *s)
{
  if (s->i++ == 1)
    {
      s->i = 0;
      s->j++;
    }
}

void
foo1 (struct S *s)
{
  bar (s);
}

void
foo2 (struct S s1, struct S s2, int i)
{
  while (s1.i != s2.i) {
    if (i)
      *(unsigned *) 0 |= (1U << s1.i);
    bar (&s1);
  }
}
