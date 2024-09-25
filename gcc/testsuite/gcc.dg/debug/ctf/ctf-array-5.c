/* CTF generation for unsized (but initialized) arrays

   In this testcase, one CTF array type record of size 5 is expected.

     Variables:
      _CTF_SECTION ->  5: const const char [5] (size 0x5) -> 4: const char [5] (size 0x5)

*/

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x5\[\t \]+\[^\n\]*cta_nelems" 1 } } */

const char _CTF_SECTION[] = ".ctf";
