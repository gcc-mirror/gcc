/* { dg-do compile } */
/* { dg-options "-std=c99" } */
/* Test that -std=c99 does not enable extended identifiers while the
   feature is experimental; remove this test after audit of all
   identifier uses in the compiler.  */
#define a b(
#define b(x) q
int a\u00aa);
