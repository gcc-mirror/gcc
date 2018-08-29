/* PR debug/83422 */
/* { dg-do compile } */
/* { dg-options "-O -g --param=max-vartrack-size=1" } */

int
foo(int i, int j, int k)
{
   return i + j + k;
}
