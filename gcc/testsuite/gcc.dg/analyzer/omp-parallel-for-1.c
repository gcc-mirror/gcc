/* { dg-require-effective-target fopenmp } */
/* { dg-additional-options "-fopenmp -Wall" } */

typedef struct _Image
{
  int columns, rows;
} Image;

extern int get_num_threads(void);

void
test (Image* image)
{
  int y;

#pragma omp parallel for schedule(static) \
  num_threads(get_num_threads ())

  for (y = 0; y < image->rows; y++) {
    /* [...snip...] */
  }
}
