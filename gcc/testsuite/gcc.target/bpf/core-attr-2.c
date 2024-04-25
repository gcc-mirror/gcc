/* Basic test for union __attribute__((preserve_access_index))
   for BPF CO-RE support.  */

/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

union U {
  int a;
  char c;
} __attribute__((preserve_access_index));

void
func (union U *u)
{
  /* 0:1 */
  char *c = &(u->c);
  *c = 'c';
}

/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 1 } } */
