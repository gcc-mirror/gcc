/* Test BTF generation for 64 bits enums.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \].size\[\t \]_?myenum1,\[\t \]8" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \].size\[\t \]_?myenum2,\[\t \]8" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \].size\[\t \]_?myenum3,\[\t \]8" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x13000003\[\t \]+\[^\n\]*btt_info" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x93000003\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xffffffaa\[\t \]+\[^\n\]*bte_value_lo32" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xff\[\t \]+\[^\n\]*bte_value_hi32" 3 } } */
/* { dg-final { scan-assembler-times "ascii \"B1.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"B2.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"B3.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"C1.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"C2.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"C3.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"D1.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"D2.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"D3.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "bte_value_lo32" 9 } } */
/* { dg-final { scan-assembler-times "bte_value_hi32" 9 } } */

enum default_enum
{
  B1 = 0xffffffffaa,
  B2 = 0xbbbbbbbb,
  B3 = 0xaabbccdd,
} myenum1 = B1;

enum explicit_unsigned
{
  C1 = 0xffffffffbbUL,
  C2 = 0xbbbbbbbb,
  C3 = 0xaabbccdd,
} myenum2 = C1;

enum signed64
{
  D1 = 0xffffffffaa,
  D2 = 0xbbbbbbbb,
  D3 = -0x1,
} myenum3 = D1;
