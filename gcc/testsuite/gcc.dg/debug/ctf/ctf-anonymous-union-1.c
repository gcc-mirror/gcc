/* Test compilation and CTF generation of anonymous union.  An anonymous union
   is encoded as no-name CTF union type.

   For this testcase, a single CTF anonymous union is expected.
   struct {} : ctt_name = 0 (point to offset 0 in the CTF string table to
			    denote empty string)
   
   Two CTF struct records should be generated in total.
   struct anon_union : ctt_info = 0x1a000001 (1 field)
   union {} : ctt_info = 0x1e000002 (2 fields)  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*ctt_name" 1 } } */
/* { dg-final { scan-assembler-times "0x1a000001\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "0x1e000002\[\t \]+\[^\n\]*ctt_info" 1 } } */

struct anon_union
{
  union
    {
      char name;
      int value;
    };
} my_anon_u;
