/* Test BTF generation of anonymous union.

   We expect a named struct type and an anonymous union type record to
   be generated. The anonymous union record should have a name of 0,
   pointing to the null string at the start of the string table.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Struct type with 1 member.  */
/* { dg-final { scan-assembler-times "\[\t \]0x4000001\[\t \]+\[^\n\]*btt_info" 1 } } */
/* Anonymous union type with 2 members.  */
/* { dg-final { scan-assembler-times "\[\t \]0x5000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_UNION ''" 1 } } */

struct foo
{
  union
    {
      int value;
      char ascii;
    };
} myfoo;
