/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  unsigned int a1: 7;
  unsigned int a2: 4;
  unsigned int a3: 13;
  unsigned int a4: 5;
  int x;
};

struct T {
  unsigned int y;
  struct S s[2];
  char c;
  char d;
};

enum {
  FIELD_BYTE_OFFSET = 0,
};


unsigned int foo (struct T *t)
{
  unsigned s0a1 = __builtin_preserve_field_info (t->s[0].a1, FIELD_BYTE_OFFSET);
  unsigned s0a4 = __builtin_preserve_field_info (t->s[0].a4, FIELD_BYTE_OFFSET);
  unsigned s0x  = __builtin_preserve_field_info (t->s[0].x, FIELD_BYTE_OFFSET);

  unsigned s1a1 = __builtin_preserve_field_info (t->s[1].a1, FIELD_BYTE_OFFSET);
  unsigned s1a4 = __builtin_preserve_field_info (t->s[1].a4, FIELD_BYTE_OFFSET);
  unsigned s1x  = __builtin_preserve_field_info (t->s[1].x, FIELD_BYTE_OFFSET);

  unsigned c = __builtin_preserve_field_info (t->c, FIELD_BYTE_OFFSET);
  unsigned d = __builtin_preserve_field_info (t->d, FIELD_BYTE_OFFSET);

  return s0a1 + s0a4 + s0x + s1a1 + s1a4 + s1x + c + d;
}

/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],4" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],8" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],12" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],16" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],20" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],21" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:1:0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:0:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:0:4.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1:4.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:2.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:3.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0\[\t \]+\[^\n\]*bpfcr_kind" 8 } } */
