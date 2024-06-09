/* Reduced from ImageMagick-7.1.0-57's MagickCore/attribute.c: GetEdgeBackgroundColor */

/* { dg-require-effective-target fopenmp } */
/* { dg-additional-options "-fopenmp -Wall" } */

extern double get_census (void);

double
test()
{
  double census[4], edge_census;
  int i;

#pragma omp parallel for schedule(static)

  for (i = 0; i < 4; i++) {
    census[i] = get_census ();
  }
  edge_census = (-1.0);
  for (i = 0; i < 4; i++)
    if (census[i] > edge_census) { /* { dg-bogus "use of uninitialized value" } */
      edge_census = census[i];
    }
  return edge_census;
}
