/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  int x;
  char c;
};

union U {
  int i;
  struct S s;
};

enum {
  FIELD_RSHIFT_U64 = 5,
};

unsigned int foo (union U *u)
{
  /* sx = 32, sc = 56, i = 32; endianness independent.  */
  unsigned sx = __builtin_preserve_field_info (u->s.x, FIELD_RSHIFT_U64);
  unsigned sc = __builtin_preserve_field_info (u->s.c, FIELD_RSHIFT_U64);
  unsigned i  = __builtin_preserve_field_info (u->i, FIELD_RSHIFT_U64);

  return sx + sc + i;
}

/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],32" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],56" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:1:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0x5\[\t \]+\[^\n\]*bpfcr_kind" 3 } } */
