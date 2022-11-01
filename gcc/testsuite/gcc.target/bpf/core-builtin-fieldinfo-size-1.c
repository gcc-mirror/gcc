/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  unsigned int a1: 7;
  unsigned int a2: 4;
  unsigned int a3: 13;
  unsigned int a4: 5;
  char carr[5][3];
};

enum {
  FIELD_BYTE_SIZE = 1,
};

union U {
  long long l[3];
  struct S s;
};

unsigned int foo (union U *u)
{
  unsigned ls = __builtin_preserve_field_info (u->l, FIELD_BYTE_SIZE);
  unsigned s  = __builtin_preserve_field_info (u->s, FIELD_BYTE_SIZE);
  unsigned a2 = __builtin_preserve_field_info (u->s.a2, FIELD_BYTE_SIZE);
  unsigned a3 = __builtin_preserve_field_info (u->s.a3, FIELD_BYTE_SIZE);
  unsigned ca = __builtin_preserve_field_info (u->s.carr, FIELD_BYTE_SIZE);

  return ls + s + a2 + a3 + ca;
}

/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],24" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],20" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],4" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],15" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:4.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0x1\[\t \]+\[^\n\]*bpfcr_kind" 5 } } */
