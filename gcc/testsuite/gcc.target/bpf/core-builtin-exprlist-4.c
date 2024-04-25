/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

struct T {
  int a;
  int b;
  struct U {
    int c;
    struct V {
      int d;
      int e[4];
      int f;
    } *v;
  } u;
};

void func (struct T * foo)
{
  /* Access string: "0:2:1:1:3" */
  int *x;
  int *y;
  __builtin_preserve_access_index (({
    x =  &((foo->u.v)->e[3]);
  }));

  *x = 17;
}

/* { dg-final { scan-assembler-times "ascii \"0:2:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 2 } } */
