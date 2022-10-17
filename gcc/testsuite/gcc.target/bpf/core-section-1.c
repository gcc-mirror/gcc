/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA -mco-re" } */

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
};

__attribute__((section("foo_sec"), used))
int foo_func (struct T *t)
{
  t->u.c = 5;
  return __builtin_preserve_access_index (t->u.v.e[3]);
}

__attribute__((section("bar_sec"), used))
int bar_func (struct T *t)
{
  int *x = __builtin_preserve_access_index (&(t->u.v.f));
  int old = *x;
  *x = 4;
  return old;
}

/* { dg-final { scan-assembler-times "ascii \"0:2:1:1:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2:1:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo_sec.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"bar_sec.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 2 } } */
/* { dg-final { scan-assembler-times "btfext_core_info_rec_size" 1 } } */
