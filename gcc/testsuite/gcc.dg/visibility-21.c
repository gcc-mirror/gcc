/* Test visibility attribute on function definition. */
/* { dg-do compile } */
/* { dg-options "-O2 -fsection-anchors" } */
/* { dg-require-visibility "" } */
/* { dg-require-weak "" } */
/* { dg-require-effective-target section_anchors } */
/* { dg-final { scan-assembler-not "ANCHOR" } } */

int __attribute__((weak, visibility("hidden"))) weak_hidden[3];

int *f_weak_hidden ()
{
  return weak_hidden;
}
