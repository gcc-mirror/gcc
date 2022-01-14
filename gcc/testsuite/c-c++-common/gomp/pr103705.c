/* PR c++/103705 */
/* { dg-do compile } */

struct S
{
  int a[2];
};

int main (void)
{
  struct S s[1];
  #pragma omp target update from(s[0].a[0:1])
  return 0;
}
