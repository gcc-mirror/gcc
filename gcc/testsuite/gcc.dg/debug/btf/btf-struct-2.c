/* Test BTF generation for struct type with a member which refers to an
   unsupported type.

   BTF does not support floating point types (among other things). When
   generating BTF for a struct (or union) type, members which refer to
   unsupported types should be skipped.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Expect a struct with only 2 members - 'f' should not be present.  */
/* { dg-final { scan-assembler-times "\[\t \]0x4000002\[\t \]+\[^\n\]*btt_info" 1 } } */

struct with_float
{
  int a;
  float __attribute__((__vector_size__(16))) f;
  char c;
} instance;
