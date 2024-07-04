/* Test BTF generation of BTF_KIND_{CONST,VOLATILE,RESTRICT} records.

   BTF const, volatile and restrict records are nameless type records pointing
   to the type they modify.

   Types:
     [1] int 'int' size=4U offset=0 bits=32 SIGNED
     [2] const <anonymous> type=1
     [3] volatile <anonymous> type=1
     [4] const <anonymous> type=3
     [5] ptr <anonymous> type=1
     [6] restrict <anonymous> type=5
     [7] ptr <anonymous> type=2
     [8] restrict <anonymous> type=7

   Note:
   - Type id 3 describes a volatile int.
   - Type id 2 describes a const int.
   - Type id 4 describes a const volatile int by modifying id 3.
   - Type id 6 describes a restrict pointer to int.
   - Type id 8 describes a restrict pointer to const int.
 */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */
/* { dg-additional-options "-gdwarf-4" { target { *-*-darwin* } } } */

/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

/* types 5 and 7 are pointers, to 'int' and 'const int' respectively.  */
/* { dg-final { scan-assembler-times "\[\t \]0x2000000\[\t \]+\[^\n\]*btt_info" 2 } } */

/* type 3 has VOLATILE qualifier */
/* { dg-final { scan-assembler-times "\[\t \]0x9000000\[\t \]+\[^\n\]*btt_info" 1 } } */

/* types 2 and 4 have CONST qualifier.  */
/* { dg-final { scan-assembler-times "\[\t \]0xa000000\[\t \]+\[^\n\]*btt_info" 2 } } */

/* types 6 and 8 have RESTRICT qualifier.  */
/* { dg-final { scan-assembler-times "\[\t \]0xb000000\[\t \]+\[^\n\]*btt_info" 2 } } */

const int a = 10;

volatile int b;

int * restrict c;

const volatile int d = 20;

const int * restrict e;

const int * f;
int const * g;
