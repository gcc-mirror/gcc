/* Test CTF generation for a typedef instantiation with CVR quals.

   Caveat: There is no direct way to test that the type of foo is
   "const my_int_t" via scanning the assembly. This test instead
   checks for the presence of some of the CTF constructs involved
   individually. Specifically, it checks for CTF const record and
   CTF typedef record.

  Variables:
      foo ->  4: const my_int_t (size 0x8) -> 3: my_int_t (size 0x8) -> 1: struct my_int (size 0x8)

   Types:
      1: struct my_int (size 0x8)
      2: int (size 0x4)
      3: my_int_t (size 0x8) -> 1: struct my_int (size 0x8)
      4: const my_int_t (size 0x8) -> 3: my_int_t (size 0x8) -> 1: struct my_int (size 0x8)
*/

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x4\[\t \]+\[^\n\]*ctv_typeidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2a000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

typedef struct my_int
{
    int upper;
      int lower;
} my_int_t;

const my_int_t foo = {10, 20};
