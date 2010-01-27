/* { dg-do compile } */
/* { dg-options "-fcompare-debug" } */

extern void error(void) __attribute__((noreturn));

struct S {
  struct S *s;
};

static inline unsigned char f2()
{
  error();
}

static inline void f1()
{
  int builtin_optab;
  if (!f2() && builtin_optab)
    error();
}

extern void f4(struct S *s);

static inline void f3(struct S *s)
{
  f4(s->s->s);
}

void expand_builtin(struct S *s, int cond)
{
  if (cond)
    f1();
  f3(s);
}
