/* CTF generation of function pointers.

   In this testcase,  there is a single function type expected.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "\[\t \]0x16000001\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo_init_callback.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"fn.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

struct foo;

typedef void (* foo_init_callback) (struct foo *f1);

struct foo
{
  /* Function to call to initialize.  */
  foo_init_callback fn;
};

struct foo f;
