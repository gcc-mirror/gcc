/* Test for BPF CO-RE __attribute__((preserve_access_index)) with accesses on
   LHS and both LHS and RHS of assignment.  */

/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -mco-re -masm=normal" } */

struct U {
  int c;
  struct V {
    int d;
    int e[4];
    int f;
    int *g;
  } v;
} u;

struct T {
  int a;
  int b;
  struct U u;
  struct U *ptr_u;
  struct U *array_u;
} __attribute__((preserve_access_index));

extern void mset(void *);

void
func (struct T *t, int i)
{
  /* 0:3    0:1:1:1   */
  t->ptr_u->v.e[1] = 0xb1;

  /* 0:3    0:1:2 */
  mset (&(t->ptr_u->v.f));

  /* 0:0 */
  mset (&t->a);
}

/* { dg-final { scan-assembler-times "ascii \"0:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:1:1:1\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:3\"\\)" 2 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:1:2\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:0\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct T\\)" 3 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct U\\)" 2 } } */

