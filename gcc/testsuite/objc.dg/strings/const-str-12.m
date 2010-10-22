/* { dg-options "-Wall -funit-at-a-time" } */
/* { dg-do compile }  */
/* PR objc/27438, make sure that the decl produced by the front-end
   does not cause a warning to be produced. */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

@interface NXConstantString
{
  void *isa;
  const char * const nxcsptr;
  const unsigned int nxcslen;
}
@end
NXConstantString *a =   @"NSInconsistentArchiveException"; /* { dg-bogus "defined but not used" } */


