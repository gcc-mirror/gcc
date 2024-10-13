/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7em_hard_ok } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-mslow-flash-data" } */
/* { dg-add-options arm_arch_v7em_hard } */

/* From PR71607 */

float b;
void fn1 ();

float
fn2 ()
{
  return 1.1f;
}

void
fn3 ()
{
  float a[2];
  a[1] = b;
  fn1 (a);
}
