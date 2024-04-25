/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -mno-co-re -masm=normal" } */

struct T {
  int a;
  int b;
  struct U {
    int c;
    struct V {
      int d;
      int e[4];
      int f;
    } v;
  } u;
} __attribute__((preserve_access_index));

__attribute__((section("foo_sec"), used))
int foo_func (struct T *t)
{
  t->u.c = 5;
  return t->u.v.e[3];
}

__attribute__((section("bar_sec"), used))
int bar_func (struct T *t)
{
  int *x = &(t->u.v.f);
  int old = *x;
  *x = 4;
  return old;
}

/* { dg-final { scan-assembler-times "FuncInfo section string for foo_sec" 1 } } */
/* { dg-final { scan-assembler-times "FuncInfo section string for bar_sec" 1 } } */
/* { dg-final { scan-assembler-times "label for function foo_func" 1 } } */
/* { dg-final { scan-assembler-times "label for function bar_func" 1 } } */
/* { dg-final { scan-assembler-times ".4byte\t0x1\t# Number of entries" 2 } } */
/* { dg-final { scan-assembler-times "Required padding" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"foo_sec.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"bar_sec.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

