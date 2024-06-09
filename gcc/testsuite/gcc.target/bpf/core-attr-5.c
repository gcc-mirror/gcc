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
} __attribute__((preserve_access_index));

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
  /* This next expression is silently generating incomplete CO-RE relocations
   * because of a front-end optimization to the array access with i.  It is
   * converting the access to pointer arithmetics, which completely removes the
   * reference to the type of array_u.  With current folding it is not possible
   * to generate correct/complete CO-RE relocations for such cases. The XFAIL
   * is exactly for this reason. Once we do XPASS this test there is a slight
   * chance we are doing the proper code generation. */

  /* 0:4   sizeof(struct U)   0:1:1:3 */
  t->array_u[i].v.e[3] = 0xc1;

  /* This was commented since in this case it is detectable as not a field
   * expression. */
  /* 0:4   sizeof(struct U)   0:1:1:2 */
  //__builtin_preserve_access_index (t->array_u[i].v.e[2]) = 0xa1;

  /* 0:3    0:1:1:1   */
  t->ptr_u->v.e[1] = 0xb1;

  /* 0:3    0:1:2 */
  mset (&(t->ptr_u->v.f));

  mset (&t->a);
}

/* { dg-final { scan-assembler-times "ascii \"0:4.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:1:1:3\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:4\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:3\"\\)" 2 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:1:2\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:1:1:1\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0:0\"\\)" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct T\\)" 4 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(struct U\\)" 4 { xfail *-*-* } } } */
