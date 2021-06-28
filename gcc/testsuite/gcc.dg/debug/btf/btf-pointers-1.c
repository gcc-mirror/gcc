/* Test BTF generation for pointer types.

   Two pointer types are expected:
    - int *
    - struct st *
   */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x2000000\[\t \]+\[^\n\]*btt_info" 2 } } */
/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"st.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

int foo = 10;
int *pfoo = &foo;

struct st
{
  int a;
  int *pb;
  struct st * next;
};

struct st * bar;
