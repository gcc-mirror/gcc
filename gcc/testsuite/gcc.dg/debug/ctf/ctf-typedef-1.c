/* CTF_K_TYPEDEF record generation.

   In this testcase, 7 typedef records are expected.

   Further, the declared variables must be of type typedef

     Variables:
	a ->  2: my_int (size 0x4) -> 1: int (size 0x4)
	b ->  3: bar_int (size 0x4) -> 1: int (size 0x4)
	c ->  4: foo_int (size 0x4) -> 1: int (size 0x4)
	d ->  7: my_array (size 0x8) -> 5: struct  (size 0x8)
	e ->  9: CINT (size 0x4) -> 8: const int (size 0x4) -> 1: int (size 0x4)
	f ->  c: CINTP (size 0x8) -> b: const int * (size 0x8) -> a: const int (size 0x4) -> 1: int (size 0x4)
	g ->  f: my_node_t (size 0x8) -> d: struct my_node (size 0x8)

    There is no direct way to check that the variables are of type typedef.
    So in this testcase, we simply check that:
    1. The typedef records are generated (Check for 7 specific ctt_info, and
       check for the ascii strings for the typedef names).
    2. The ctv_typeidx are distinct (each pointing to a specfic unique type).
       Note that if variables were not of type typedef, ctv_typeidx will not be
       unique (type of a, b, c will all point to int); hence, the check.
   */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x2a000000\[\t \]+\[^\n\]*ctt_info" 7 } } */
/* { dg-final { scan-assembler-times "ascii \"my_int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"bar_int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo_int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"my_array.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"CINT.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"CINTP.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"my_node_t.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x4\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x5\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x9\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xa\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xc\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xf\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */

typedef int my_int;
typedef int bar_int;
typedef int foo_int;

typedef struct { int a[2]; } my_array;

typedef const int CINT;
typedef const int * CINTP;

typedef struct my_node
{
  int flags;
  char value;
} my_node_t;

my_int a;
bar_int b;
foo_int c;

my_array d;
CINT e = 3;
CINTP f = &e;

my_node_t g;
