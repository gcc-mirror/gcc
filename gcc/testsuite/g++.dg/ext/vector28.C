/* { dg-do compile } */

typedef int int32_t __attribute__((mode (__SI__)));

typedef int veci __attribute__ ((vector_size (4 * sizeof (int32_t))));
typedef float vecf __attribute__ ((vector_size (4 * sizeof (float))));

void f (veci *a, vecf *b, int c)
{
  *a = c || *b;
  *a = *a || c;
}
