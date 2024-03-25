/* PR target/112837 */
/* { dg-do compile } */
/* { dg-options "-fcompare-elim -fprofile" } */
/* { dg-additional-options "-fpie" { target pie } } */
/* { dg-require-profiling "-fprofile" } */

void
foo (int i)
{
  foo (i);
}
