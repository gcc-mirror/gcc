/* Test compilation of stubs with various expressions involving const
   qualifier.
   
   In this testcase, a single CTF record for const int is expected. A total of
   two const qualifier CTF records are expected (const int and const struct
   s1).  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

struct mystruct
{
  struct
    {
      int a; 
      const int b;
    } s1;
  char * name;
} my_a;

struct s1
{
  int i;
  const int ci;
} s;

const struct s1 cs;
