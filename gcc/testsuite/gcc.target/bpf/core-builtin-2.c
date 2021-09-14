/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  int a;
  union {
    int _unused;
    int b;
    char c;
  } u[4];
};

struct S foo;

#define _(x) (__builtin_preserve_access_index (x))

void func (void)
{
  char *x = __builtin_preserve_access_index (&foo.u[3].c);

  *x = 's';
}

/* { dg-final { scan-assembler-times "\[\t \]0x4000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"1:3:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 1 } } */
