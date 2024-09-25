/* CTF generation for pointer types.

   In this testcase, two CTF pointer type records are expected
     - int *
     - struct foo_struct *
  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"foo_struct.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

int b = 44;
int * a = &b;

struct foo_struct
{
  int bar_mem_1;
  int bar_mem_2;
  float d;
  struct foo_struct *next;
};

struct foo_struct * node;
