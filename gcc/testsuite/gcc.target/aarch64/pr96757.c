/* PR target/96757 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

short 
fun1(short i, short j)
{ 
  return i * j; 
}

int 
fun(int a, int b, int c) 
{
  int *v, z, k, m;
  short f, d;
  for (int i=0; i<c; i++) 
  {
    f= 4 <= d;
    k= a > m;
    z = f > k;
    *v += fun1(z,b);
  }
}
