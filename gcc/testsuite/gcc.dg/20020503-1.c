/* PR target/6542
   This testcase caused ICE on SPARC because the function uses no registers
   after optimizing, so even if -mflat make all registers not permitted
   for leaf functions, the function was still leaf, but LEAF_REG_REMAP
   returned -1 for some registers (like %o0).  */
/* { dg-do compile } */
/* { dg-options "-O2 -g -mflat" { target sparc*-*-* } } */

void foo (char *a, char *b, char *c, char *d)
{
}
