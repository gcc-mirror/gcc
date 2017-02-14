/* Verify that the DW_AT_producer does not contain certain compiler options
   such as -fdebug-prefix-map=; this is undesirable since path names make
   the build not reproducible.  Other skipped options could be tested here
   as well.  */
/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf -dA -fdebug-prefix-map=a=b" } */
/* { dg-final { scan-assembler "DW_AT_producer: \"GNU C" { target { { { ! *-*-solaris2* } || gas } && { { ! hppa*64*-*-* } && { ! powerpc-ibm-aix* } } } } } } */
/* { dg-final { scan-assembler "\"GNU C\[^\\n\\r\]+ DW_AT_producer" { target { { *-*-solaris2* && { ! gas } } || { hppa*64*-*-* } } } } } */
/* { dg-final { scan-assembler-not "debug-prefix-map" } } */

void func (void)
{
}
