/* Test to make sure CO-RE access relocs point to non const versions of the
   type.  */

/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

struct S {
  int a;
  int b;
  int c;
} __attribute__((preserve_access_index));

void
func (struct S * s)
{
  int *x;
  int *y;
  int *z;
  struct S tmp;
  const struct S *cs = s;
  volatile struct S *vs = s;

  /* 0:2 */
  x = &(s->c);

  /* 0:1 */
  y = (int *) &(cs->b);

  /* 0:1 */
  z = (int *) &(vs->b);

  *x = 4;
  *y = 4;
  *z = 4;
}

/* Both const and non const struct type should have the same bpfcr_type. */
/* { dg-final { scan-assembler-times "0x1\t# bpfcr_type \\(struct S\\)" 1 } } */
/* { dg-final { scan-assembler-times "0x1\t# bpfcr_type \\(const struct S\\)" 1 } } */
/* { dg-final { scan-assembler-times "0x1\t# bpfcr_type \\(volatile struct S\\)" 1 } } */
