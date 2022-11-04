/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct S {
  char c;
  short s;
  int x;
};

union U {
  struct S s[2];
  long long ll;
};

enum {
  FIELD_LSHIFT_U64 = 4,
};

unsigned int foo (union U *u)
{
  /* s0s = 48, s1c = 56, ll = 0; endianness independent.  */
  unsigned s0s = __builtin_preserve_field_info (u->s[0].s, FIELD_LSHIFT_U64);
  unsigned s1c = __builtin_preserve_field_info (u->s[1].c, FIELD_LSHIFT_U64);
  unsigned ll  = __builtin_preserve_field_info (u->ll, FIELD_LSHIFT_U64);

  return s0s + s1c + ll;
}

/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],48" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],56" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],0" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:0:0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:0:1:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "0x4\[\t \]+\[^\n\]*bpfcr_kind" 3 } } */
