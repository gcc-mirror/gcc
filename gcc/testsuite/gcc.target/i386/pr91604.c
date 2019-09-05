/* PR target/91604 */
/* { dg-do compile } */
/* { dg-options "-O3 -msse2 --param max-gcse-memory=0 -fno-rerun-cse-after-loop" } */

long long v;

void
foo (void)
{
  v = ~v;
}
