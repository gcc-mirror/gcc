/* Test for __attribute__((preserve_access_index)) for BPF CO-RE support
   for nested structure.

   Note that even though struct O lacks the attribute, when accessed as a
   member of another attributed type, CO-RE relocations should still be
   generated.  */

/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct O {
  int e;
  int f;
};

struct S {
  int a;
  struct {
    int b;
    int c;
  } inner;
  struct O other;
} __attribute__((preserve_access_index));

void
func (struct S *foo)
{
  /* 0:1:1 */
  int *x = &(foo->inner.c);

  /* 0:2:0 */
  int *y = &(foo->other.e);

  *x = 4;
  *y = 5;
}

/* { dg-final { scan-assembler-times "ascii \"0:1:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "bpfcr_type" 2 } } */
