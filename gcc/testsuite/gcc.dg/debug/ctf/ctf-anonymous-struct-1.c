/* Test compilation and CTF generation of anonymous structs.  An anonymous
   struct type is encoded as no-name CTF struct type.

   For this testcase, a single CTF anonymous struct is expected.
   struct {} : ctt_name = 0 (point to offset 0 in the CTF string table to
			    denote empty string)
   
   Two CTF struct records should be generated in total.
   struct a : ctt_info = 0x1a000002 (2 fields)
   struct {} : ctt_info = 0x1a000001 (1 field)  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*ctt_name" 1 } } */
/* { dg-final { scan-assembler-times "0x1a000002\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "0x1a000001\[\t \]+\[^\n\]*ctt_info" 1 } } */

struct a
{
  struct { int b1; } a1;
  int a2;
} my_a;
