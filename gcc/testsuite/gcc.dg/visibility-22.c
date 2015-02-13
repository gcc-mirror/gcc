/* PR target/32219 */
/* { dg-do run } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2 -fPIC" { target fpic } } */
/* This test requires support for undefined weak symbols.  This support
   is not available on hppa*-*-hpux*.  The test is skipped rather than
   xfailed to suppress the warning that would otherwise arise.  */
/* { dg-skip-if "" { "hppa*-*-hpux*" "*-*-aix*" "*-*-darwin*" } "*" { "" } }  */

extern void foo () __attribute__((weak,visibility("hidden")));
int
main()
{
  if (foo)
    foo ();
  return 0;
}
