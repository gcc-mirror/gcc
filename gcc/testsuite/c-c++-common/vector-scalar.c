/* { dg-do compile } */

typedef float vecf __attribute__ ((vector_size (4 * sizeof (float))));
typedef short veci __attribute__ ((vector_size (8 * sizeof (short))));

void f (vecf *d, veci *i)
{
  (void) ((*d *= 2) < 0);
  (void) ((((*i - 1) >> 2) != 0) | *i);
}
