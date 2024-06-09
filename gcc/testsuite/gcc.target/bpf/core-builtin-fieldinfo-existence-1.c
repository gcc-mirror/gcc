/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

enum {
  FIELD_EXISTENCE = 2,
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
  unsigned c  = __builtin_preserve_field_info (s->c, FIELD_EXISTENCE);
  unsigned d  = __builtin_preserve_field_info (s->d, FIELD_EXISTENCE);
  unsigned u  = __builtin_preserve_field_info (s->u, FIELD_EXISTENCE);
  unsigned ar = __builtin_preserve_field_info (s->ar[1], FIELD_EXISTENCE);

  return c + d + u + ar;
}

/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],1" 4 } } */

/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:3:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0x2\[\t \]+\[^\n\]*bpfcr_kind" 4 } } */
