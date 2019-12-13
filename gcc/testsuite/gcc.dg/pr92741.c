/* { dg-options "-O2 -fexceptions -fnon-call-exceptions -fno-inline" } */

typedef int vh __attribute__ ((__vector_size__ (2 * sizeof (int))));
typedef short int cq __attribute__ ((__vector_size__ (4 * sizeof (short int))));

static void
id (int *r8, vh *tu)
{
  *(vh *) r8 = *tu;
}

void
mr (void)
{
  int r8;
  cq he = { 0, };

  id (&r8, (vh *) &he);
}
