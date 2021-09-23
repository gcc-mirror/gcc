/* Test compilation of stubs with various qualifiers - const, restrict and
   volatile.

   Testcase includes a std header to allow testing of shared types across
   files.  Only one CTF record for int is expected.
   
   CTF records for CVR qualifiers are no-name records.  In this testcase, there
   are 5 qualifiers across constructs.  2 more no-name CTF records correspond to
   CTF pointer records.

    TYPEID: name string (size) -> ref TYPEID : ref name string (size) -> ...

   Types:
      1: long int (size 0x8)
      2: long unsigned int (size 0x8)
      3: size_t (size 0x8) -> 2: long unsigned int (size 0x8)
      4: int (size 0x4)
      5: const int (size 0x4) -> 4: int (size 0x4)
      6: volatile const int (size 0x4) -> 5: const int (size 0x4) -> 4: int (size 0x4)
      7: long long int (size 0x8)
      8: long double (size 0x10)
      9: int * (size 0x8) -> 4: int (size 0x4)
      a: int *restrict (size 0x8) -> 9: int * (size 0x8) -> 4: int (size 0x4)
      b: const int * (size 0x8) -> 5: const int (size 0x4) -> 4: int (size 0x4)
      c: const int *restrict (size 0x8) -> b: const int * (size 0x8) -> 5: const int (size 0x4) -> 4: int (size 0x4)
      d: INTP (size 0x8) -> 9: int * (size 0x8) -> 4: int (size 0x4)
      e: const INTP (size 0x8) -> d: INTP (size 0x8) -> 9: int * (size 0x8) -> 4: int (size 0x4)
      f: void (size 0x0)
      10: void (*) (size_t, int *restrict, const int *restrict) (size 0x0)
    */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-options "-O0 -gctf -gdwarf-4 -dA" { target { *-*-darwin* } } } */

/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*ctt_name" 7 } } */

/* type id 9, b have POINTER type.  */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

/* type id 5, e have CONST qualifier.  */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

/* type id a, c have RESTRICT qualifier.  */
/* { dg-final { scan-assembler-times "\[\t \]0x36000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

/* type id 6 has VOLATILE qualifier.  */
/* { dg-final { scan-assembler-times "\[\t \]0x2e000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

#include "stddef.h"

const volatile int a = 5;
int *restrict b;

const int * i;
int const * j;

typedef int * INTP;
const INTP int_p;

void foo (size_t n, int *restrict p, const int *restrict q)
{
  while (n-- > 0)
    *p++ = *q++;
}
