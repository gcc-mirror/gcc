/* Tests for BTF integer base types.

     0       f       ff      00   ff
   | 0 | encoding | offset | 00 | bits |
   encoding:
     signed  1 << 24
     char    2 << 24

   All offsets in this test should be 0.
   This test does _not_ check number of bits, as it may vary between targets.
 */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Check for 8 BTF_KIND_INT types.  */
/* { dg-final { scan-assembler-times "\[\t \]0x1000000\[\t \]+\[^\n\]*btt_info" 8 } } */

/* Check the signed/char flags, but not bit size. */
/* { dg-final { scan-assembler-times "\[\t \]0x10000..\[\t \]+\[^\n\]*bti_encoding" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x20000..\[\t \]+\[^\n\]*bti_encoding" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x30000..\[\t \]+\[^\n\]*bti_encoding" 1 } } */

/* Check that there is a string entry for each type name.  */
/* { dg-final { scan-assembler-times "ascii \"unsigned char.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"signed char.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"short unsigned int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"short int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"unsigned int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"long unsigned int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"long int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

unsigned char a = 11;
signed char b = -22;

unsigned short c = 33;
signed short d = 44;

unsigned int e = 55;
signed int f = -66;

unsigned long int g = 77;
signed long int h = 88;
