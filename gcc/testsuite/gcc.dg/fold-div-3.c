/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

unsigned int
apply_frontend_param (unsigned int spi_bias)
{
  static const int ppm = 8000;
  spi_bias /= 1000ULL + ppm/1000;
  return spi_bias;
}

/* Make sure we perform the division in the narrower type.  */

/* { dg-final { scan-tree-dump "spi_bias = spi_bias / 1008;" "original" } } */
