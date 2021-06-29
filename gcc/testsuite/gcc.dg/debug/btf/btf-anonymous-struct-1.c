/* Test BTF generation of anonymous struct.

   We expect two BTF struct records:
   - struct foo, with two fields "a" and "bar"
   - struct <anonymous> with one field "b"

   The anonymous struct should have a name of 0, pointing to the null string
   at the start of the string table.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Struct type with 2 members (struct foo).  */
/* { dg-final { scan-assembler-times "\[\t \]0x4000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* Struct type with 1 member (anon struct).  */
/* { dg-final { scan-assembler-times "\[\t \]0x4000001\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*btt_name" 1 } } */

struct foo
{
  int a;
  struct { int b; } bar;
} myfoo;
