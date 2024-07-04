/* Basic test for struct __attribute__((preserve_access_index))
   for BPF CO-RE support.  */

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
  /* This test is marked as XFAIL since for the time being the CO-RE
     implementation is not able to disambiguate between a point manipulation
     and a CO-RE access when using preserve_access_index attribute.  The
     current implemetantion is incorrect if we consider that STRUCT S might
     have different size within the kernel.
     This example demonstrates how the implementation of preserve_access_index
     as an attribute of the type is flagile.  */

  /* 2:2 */
  int *x = &((s+2)->c);
  *x = 4;

  /* 2:1 */
  int *y = __builtin_preserve_access_index (&((s+2)->b));
  *y = 2;
}

/* { dg-final { scan-assembler-times "ascii \"2:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times "ascii \"2:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "bpfcr_type" 2 } } */
