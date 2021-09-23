/* CTF does not have representation for _Atomic qualifier.  This qualifier is
   skipped in the CTF generation phase in the compiler.

   In this testcase, CTF records for the _Atomic qualifier are not added as
   CTF has no representation for it. CTF records for the underlying type are,
   however, added.  So, CTF records for typedef, const and the underlying
   struct are expected.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x2a000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1a000003\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ctm_name" 3 } } */

/* { dg-final { scan-assembler-times "ascii \"comp_type.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"comp_type_t.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"c1.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
typedef struct comp_type
{
  int a;
  float b;
  char c;
} comp_type_t;

_Atomic const comp_type_t c1;
