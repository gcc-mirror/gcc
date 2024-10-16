/* PR middle-end/63762 */
/* { dg-do assemble } */
/* { dg-options "-std=gnu17 -O2" } */

#include <stdlib.h>

void *astFree ();
void *astMalloc ();
void astNegate (void *);
int astGetNegated (void *);
void astGetRegionBounds (void *, double *, double *);
int astResampleF (void *, ...);

extern int astOK;

int
MaskF (int inside, int ndim, const int lbnd[], const int ubnd[],
       float in[], float val)
{

  void *used_region;
  float *c, *d, *out, *tmp_out;
  double *lbndgd, *ubndgd;
  int *lbndg, *ubndg, idim, ipix, nax, nin, nout, npix, npixg, result = 0;
  if (!astOK) return result;
  lbndg = astMalloc (sizeof (int)*(size_t) ndim);
  ubndg = astMalloc (sizeof (int)*(size_t) ndim);
  lbndgd = astMalloc (sizeof (double)*(size_t) ndim);
  ubndgd = astMalloc (sizeof (double)*(size_t) ndim);
  if (astOK)
    {
      astGetRegionBounds (used_region, lbndgd, ubndgd);
      npix = 1;
      npixg = 1;
      for (idim = 0; idim < ndim; idim++)
        {
          lbndg[ idim ] = lbnd[ idim ];
          ubndg[ idim ] = ubnd[ idim ];
          npix *= (ubnd[ idim ] - lbnd[ idim ] + 1);
          if (npixg >= 0) npixg *= (ubndg[ idim ] - lbndg[ idim ] + 1);
        }
      if (npixg <= 0 && astOK)
        {
          if ((inside != 0) == (astGetNegated( used_region ) != 0))
            {
              c = in;
              for (ipix = 0; ipix < npix; ipix++) *(c++) = val;
              result = npix;
            }
        }
      else if (npixg > 0 && astOK)
        {
          if ((inside != 0) == (astGetNegated (used_region) != 0))
            {
              tmp_out = astMalloc (sizeof (float)*(size_t) npix);
              if (tmp_out)
                {
                  c = tmp_out;
                  for (ipix = 0; ipix < npix; ipix++) *(c++) = val;
                  result = npix - npixg;
                }
              out = tmp_out;
            }
          else
            {
              tmp_out = NULL;
              out = in;
            }
          if (inside) astNegate (used_region);
          result += astResampleF (used_region, ndim, lbnd, ubnd, in, NULL,
                                  NULL, NULL, 0, 0.0, 100, val, ndim,
                                  lbnd, ubnd, lbndg, ubndg, out, NULL);
          if (inside) astNegate (used_region);
        }
    }
  return result;
}
