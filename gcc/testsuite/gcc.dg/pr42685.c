/* { dg-do compile } */
/* { dg-options "-O -funroll-loops -fcompare-debug" } */

struct S {
 int i;
};

extern void baz(int);

static inline void bar(struct S *s)
{
  baz(s->i);
}

void foo(int *w, int cond, struct S *s)
{
  int i, n = 0;
  while (*w++ != 0) n++;
  for (i = 0; i < n; i++)
    if (cond == 0)
      bar(s + i);
}
