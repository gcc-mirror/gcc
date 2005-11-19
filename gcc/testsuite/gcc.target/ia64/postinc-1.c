/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mlp64" { target ia64-*-hpux* } } */

void copy_loop_ldouble (void *xdest,
                        const void *xsrc,
                        long roff,
                        long soff,
                        long len,
                        long shift)
{ __float128 *dest = xdest;
  const long double *src;
  long i;
  roff /= sizeof (__float128);
  soff /= sizeof (__float128);
  src = xsrc;
  src += shift * soff;
  for (i = 0; i < len - shift; ++i) {
        *dest = *src;
        dest += roff;
        src += soff;
  }
}
