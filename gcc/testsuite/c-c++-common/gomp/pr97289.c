/* PR middle-end/97289 */
/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-skip-if "" { "hppa*-*-hpux*" "*-*-aix*" "nvptx-*-*" } } */

void foo (void);
static void bar (void) __attribute__ ((__weakref__ ("foo")));

void
baz (void)
{
#pragma omp target
  bar ();
}
