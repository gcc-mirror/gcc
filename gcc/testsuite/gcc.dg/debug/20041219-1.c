/* PR 18067 */
/* { dg-do compile } */

void foo(int i)
{
    const int j=i+1;
    int a[1][j*j];
}
