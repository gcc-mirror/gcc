/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -mbig-endian -masm=normal" } */

struct S {
  int x1: 6;
  int x2: 3;
  int x3: 7;
  int x4: 16;
};

enum {
  FIELD_LSHIFT_U64 = 4,
};

unsigned int foo (struct S *s)
{
  /* little endian: x1=58, x2=55, x3=48, x4=32 */
  /* big endian:    x1=32, x2=38, x3=41, x4=48 */
  unsigned x1 = __builtin_preserve_field_info (s->x1, FIELD_LSHIFT_U64);
  unsigned x2 = __builtin_preserve_field_info (s->x2, FIELD_LSHIFT_U64);
  unsigned x3 = __builtin_preserve_field_info (s->x3, FIELD_LSHIFT_U64);
  unsigned x4 = __builtin_preserve_field_info (s->x4, FIELD_LSHIFT_U64);

  return x1 + x2 + x3 + x4;
}

/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],32" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],38" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],41" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],48" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0x4\[\t \]+\[^\n\]*bpfcr_kind" 4 } } */
