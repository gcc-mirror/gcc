/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

enum {
  FIELD_SIGNEDNESS = 3,
};

enum Esig {
  SA = -1,
  SB,
  SC,
};

enum Eun {
  UA = 0,
  UB,
};

struct S {
  enum Esig sig : 3;
  enum Eun un : 3;
};

union U {
  int i;
  struct S s;
};

unsigned int foo (union U *u)
{
  unsigned i   = __builtin_preserve_field_info (u->i, FIELD_SIGNEDNESS);
  unsigned sig = __builtin_preserve_field_info (u->s.sig, FIELD_SIGNEDNESS);
  unsigned un  = __builtin_preserve_field_info (u->s.un, FIELD_SIGNEDNESS);

  return i + sig + un;
}

/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],1" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]mov\[\t \]%r\[0-9\],0" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"0:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:0.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"0:1:1.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

/* { dg-final { scan-assembler-times "3\[\t \]+\[^\n\]*bpfcr_kind" 3 } } */
