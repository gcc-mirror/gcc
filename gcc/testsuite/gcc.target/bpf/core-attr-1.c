/* Basic test for struct __attribute__((preserve_access_index))
   for BPF CO-RE support.  */

/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  int a;
  int b;
  int c;
} __attribute__((preserve_access_index));

void
func (struct S * s)
{
  /* 0:2 */
  int *x = &(s->c);

  *x = 4;
}

/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 1 } } */
