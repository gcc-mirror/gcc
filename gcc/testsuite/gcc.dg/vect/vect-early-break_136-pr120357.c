/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-O3" } */

char a;
unsigned long long t[2][22];
int u[22];
void f(void)
{
  for (int v = 0; v < 22; v++)
    for (_Bool w = 0; w < (u[v] < 0) + 1; w = 1)
      a *= 0 != t[w][v];
}
