/* Type de-duplication of CTF_K_TYPEDEF records.

   In this testcase, a single CTF record for typedef is expected.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x2a000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo_type.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

typedef struct foo foo_type;

struct bar
{
  struct foo * f1;
  foo_type * f2;
};

struct testme {
    struct bar * b1;
};

struct testme * t1;
