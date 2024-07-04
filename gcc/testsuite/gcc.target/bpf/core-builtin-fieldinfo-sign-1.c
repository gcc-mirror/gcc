/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

enum {
  FIELD_SIGNEDNESS = 3,
};

typedef unsigned uint;

struct S {
  unsigned char c;
  int d;
  uint u;
  short ar[3];
};

unsigned int foo (struct S *s)
{
  unsigned d  = __builtin_preserve_field_info (s->d, FIELD_SIGNEDNESS);
  unsigned u  = __builtin_preserve_field_info (s->u, FIELD_SIGNEDNESS);
  unsigned ar = __builtin_preserve_field_info (s->ar[1], FIELD_SIGNEDNESS);

  return d + u + ar;
}

/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],1" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],0" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:3:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0x3\[\t \]+\[^\n\]*bpfcr_kind" 3 } } */
