/* PR target/32219 */
/* { dg-do run } */
/* { dg-require-visibility "" } */
/* { dg-require-effective-target weak_undefined } */
/* { dg-options "-O2 -fPIC" { target fpic } } */
/* { dg-add-options weak_undefined } */

extern void foo () __attribute__((weak,visibility("hidden")));
int
main()
{
  if (foo)
    foo ();
  return 0;
}
