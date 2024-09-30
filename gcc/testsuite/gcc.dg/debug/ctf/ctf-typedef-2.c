/* CTF_K_TYPEDEF record generation.

   In this testcase, typedef of type void should be generated.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x2a000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo_void_type.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"void.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

typedef void foo_void_type;

struct bar
{
  int a;
  foo_void_type *b;
};

struct bar c;
