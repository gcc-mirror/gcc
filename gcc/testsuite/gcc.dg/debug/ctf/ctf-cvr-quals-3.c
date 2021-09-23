/* Test compilation of stubs with various expressions involving const and
   volatile qualifiers.
   
   In this testcase, it is expected to have const and volatile CTF
   records.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"unsigned char.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2e000000\[\t \]+\[^\n\]*ctt_info" 5 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 3 } } */
/* Two arrays.  */
/* { dg-final { scan-assembler-times "\[\t \]0x12000000\[\t \]+\[^\n\]*ctt_info" 2 } } */


const volatile unsigned char  vicar = 11;

const volatile unsigned char * vicarage = &vicar;

volatile float vilify[2];

const volatile char victor = 'Y';

const volatile char vindictive[2];
