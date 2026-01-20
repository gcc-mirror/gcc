/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

struct a {
  int c[2];
};
struct a d[3];
double f;
void g()
{
  for (int e = 0; e < 3; ++e)
    f += d[e].c[1];
}
