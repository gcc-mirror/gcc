/* PR target/94292 */
/* { dg-do compile } */
/* { dg-options "-O1 -g -fno-tree-dce" } */

unsigned short a;
unsigned long long b;

long long
foo (int d)
{
  d >>= a != (unsigned long long) -a;
  return a + b;
}
