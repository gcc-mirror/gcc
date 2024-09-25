/* Test CTF generation works well with ((mode)) attribute.

   In this testcase, CTF should report type of bqi to be an enum and
   not an int.  Also, CTF for typedef of enum should exist.  However, there
   are no direct and portable methods of checking that a CTF type / CTF
   variable refers to a specific CTF type, so this testcase merely asserts
   for existence of individual CTF records.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"B1.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"B2.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"B3.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "cte_value" 3} } */

/* There are no better/direct methods to assert that the CTF for typedef of
   enum has been added.  */
/* { dg-final { scan-assembler-times "\[\t \]0x22000003\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2a000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

typedef enum { B1 = 1, B2 = 2, B3 = 3 } B;
B __attribute__ ((mode (QI))) bqi;
