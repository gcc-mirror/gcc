/* { dg-do compile } */
/* { dg-options "-fcompare-debug" } */

struct S {
  int i;
};

extern struct S *e1(void);
extern int e2(int i);

static inline void f1()
{
  int i;
  struct S *s;
  for (i = 0; i < 10; i++)
    s = e1();
  e2(s->i);
}

static inline void f2(int i)
{
  int j = j;
  j = e2(i);
}

void foo(int i)
{
  f1();
  f2(i);
}
