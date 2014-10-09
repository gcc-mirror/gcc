/* { dg-do compile } */

typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));
typedef float vecf __attribute__ ((vector_size (4 * sizeof (float))));

void f (veci *a, veci *b, int c)
{
  *a = !*a || *b < ++c;
}
void g (vecf *a, vecf *b)
{
  *a = (*a < 1 && !(*b > 2)) ? *a + *b : 3;
}
