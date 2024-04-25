/* Test for BPF CO-RE __attribute__((preserve_access_index)) with accesses on
   LHS and both LHS and RHS of assignment.  */

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
    } v;
  } u;
} __attribute__((preserve_access_index));


void
func (struct T *t)
{
  /* 0:2:1:1:3 */
  t->u.v.e[3] = 0xa1;

  /* 0:2:0, 0:0, 0:1 */
  t->u.c = t->a + t->b;
}

/* { dg-final { scan-assembler-times "ascii \"0:2:1:1:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 4 } } */
