/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -masm=normal" } */

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

/* { dg-final { scan-assembler-times "LineInfo section string for foo_sec" 1 } } */
/* { dg-final { scan-assembler-times "LineInfo section string for bar_sec" 1 } } */
/* { dg-final { scan-assembler-times "label for function foo_func" 1 } } */
/* { dg-final { scan-assembler-times "label for function bar_func" 1 } } */


/* { dg-final { scan-assembler-times "btfext-lineinfo\.c.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "4byte\[\t \]+\.LFB.\[\t \]+# insn_label" 2 } } */
/* { dg-final { scan-assembler-times "4byte\[\t \]+LI.\[\t \]+# insn_label" 6 } } */

/* { dg-final { scan-assembler-times "# \\(line, col\\)" 8 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(18, 5\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(20, 10\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(21, 18\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(22, 1\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(25, 5\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(28, 7\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(29, 6\\)" 1 } } */
/* { dg-final { scan-assembler-times "# \\(line, col\\) \\(31, 1\\)" 1 } } */
