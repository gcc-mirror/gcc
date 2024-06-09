/* Test BTF generation for enums.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -gno-strict-dwarf -fno-short-enums -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x6000004\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x86000003\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times " ENUM_CONST 'QAD' idx=0" 1 } } */
/* { dg-final { scan-assembler-times " ENUM_CONST 'QED' idx=1" 1 } } */
/* { dg-final { scan-assembler-times " ENUM_CONST 'QOD' idx=2" 1 } } */
/* { dg-final { scan-assembler-times " ENUM_CONST 'QUD' idx=3" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"QAD.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"QED.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"QOD.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"QUD.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"YES.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"NO.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"IDUNNO.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "bte_value" 7 } } */

enum foo
{
  QAD,
  QED,
  QOD,
  QUD,
} a;

enum barsigned
{
  YES=1000,
  NO=-1000,
  IDUNNO=0,
} b;
