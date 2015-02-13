/* PR target/32219 */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "foo" } } */
/* { dg-options "-O2 -fPIC" { target fpic } } */
/* { dg-skip-if "" { "hppa*-*-hpux*" "*-*-aix*" "*-*-darwin*" } "*" { "" } }  */

extern void foo () __attribute__((weak,visibility("hidden")));
int
main()
{
  if (foo)
    foo ();
  return 0;
}
