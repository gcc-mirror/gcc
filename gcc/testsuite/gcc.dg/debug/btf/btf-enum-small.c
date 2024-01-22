/* Test BTF generation for small enums.  */

/* { dg-do compile } */
/* { dg-options "-O2 -gbtf -gno-strict-dwarf -dA" } */

/* { dg-final { scan-assembler-not "bte_value_lo32" } } */
/* { dg-final { scan-assembler-not "bte_value_hi32" } } */
/* { dg-final { scan-assembler-times "\[\t \]0x6000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times " ENUM_CONST 'eSMALL' idx=0" 1 } } */
/* { dg-final { scan-assembler-times " ENUM_CONST 'eSMALLY' idx=1" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"eSMALL.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"eSMALLY.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "bte_value" 2 } } */

enum smalled_enum
{
  eSMALL,
  eSMALLY,
} __attribute__((mode(byte)));

struct root_struct {
  enum smalled_enum esmall;
};

enum smalled_enum
foo(struct root_struct *root) {
  return root->esmall;
}
