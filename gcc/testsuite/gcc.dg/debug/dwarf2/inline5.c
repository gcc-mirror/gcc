/* Verify that the concrete instance DW_TAG_lexical_block has an abstract
   origin.  Verify that the inline instance has the abstract instance as
   abstract origin rather than the concrete one.  */
/* { dg-options "-O -gdwarf -dA" } */
/* { dg-do compile } */
/* { dg-final { scan-assembler-times "DW_TAG_inlined_subroutine" 2 } } */
/* { dg-final { scan-assembler-times "DW_TAG_lexical_block\\)\[^#/!@;\\|\]*\[#/!@;\\|\]+ +DW_AT_abstract_origin" 2 } } */
/* { dg-final { scan-assembler-times "DW_TAG_lexical_block\\)\[^#/!@;\\|\]*\[#/!@;\\|\]+ +\[^#/!@\\|\]*\\(DIE \\(0x\[0-9a-f\]*\\) DW_TAG_variable" 1 { xfail hppa*-*-* } } } */
/* We do not know which is output first so look for both invalid abstract
   origins on the lexical blocks (knowing that the abstract instance has
   no attribute following the DW_TAG_lexical_block.  */
/* { dg-final { scan-assembler-not "\\(DIE \\(0x(\[0-9a-f\]*)\\) DW_TAG_lexical_block\\)\[^#/!@;\\|\]*\[#/!@;\\|\]+ +\[^(\].*DW_TAG_lexical_block\\)\[^#/!@;\\|x\]*x\\1\[^#/!@;\\|\]*\[#/!@;\\|\] +DW_AT_abstract_origin" { xfail { *-*-solaris2.* && { ! gas } } } } } */
/* { dg-final { scan-assembler-not "DW_TAG_lexical_block\\)\[^#/!@;\\|x\]*x(\[0-9a-f\]*)\[^#/!@;\\|\]*\[#/!@;\\|\]+ +DW_AT_abstract_origin.*\\(DIE \\(0x\\1\\) DW_TAG_lexical_block\\)\[^#/!@;\\|\]*\[#/!@;\\|\]+ +DW_AT" } } */

int foo (int i)
{
    {
      volatile int j = i + 3;
      return j - 2;
    }
}
int main()
{
  volatile int z = foo (-1);
  return z;
}
