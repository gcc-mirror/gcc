/* PR target/94088 */
/* { dg-do compile } */
/* { dg-options "-mtbm -O1 -fira-loop-pressure -fno-dce" } */

double
foo (int x)
{
  return x / (4294950402U % -65472 + 161);
}
