/* Exercise nested function decomposition, gcc/tree-nested.c.  */

/* { dg-additional-options "-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_loop 0] }
   { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

int
main (void)
{
  int j = 0, k = 6, l = 7, m = 8;
  void simple (void)
  {
    int i;
#pragma acc parallel
    {
#pragma acc loop /* { dg-line l_loop[incr c_loop] } */
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop$c_loop } */
      for (i = 0; i < m; i+= k)
	j = (m + i - j) * l;
    }
  }
  void collapse (void)
  {
    int x, y, z;
#pragma acc parallel
    {
#pragma acc loop collapse (3) /* { dg-line l_loop[incr c_loop] } */
      /* { dg-note {variable 'z' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-note {variable 'y' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop$c_loop } */
      for (x = 0; x < k; x++)
	for (y = -5; y < l; y++)
	  for (z = 0; z < m; z++)
	    j += x + y + z;
    }
  }
  void reduction (void)
  {
    int x, y, z;
#pragma acc parallel reduction (+:j)
    {
#pragma acc loop reduction (+:j) collapse (3) /* { dg-line l_loop[incr c_loop] } */
      /* { dg-note {variable 'z' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-note {variable 'y' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
      /* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop$c_loop } */
      for (x = 0; x < k; x++)
	for (y = -5; y < l; y++)
	  for (z = 0; z < m; z++)
	    j += x + y + z;
    }
  }
  simple();
  collapse();
  reduction();
  return 0;
}
