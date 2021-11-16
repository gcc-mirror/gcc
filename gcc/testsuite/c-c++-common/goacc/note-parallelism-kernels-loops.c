/* Test the output of "-fopt-info-optimized-omp" for an OpenACC 'kernels'
   construct containing loops.  */

/* { dg-additional-options "-fno-openacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fopt-info-note-optimized-omp" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

int
main ()
{
  int x, y, z;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. region in OpenACC .kernels. construct" } */
    ;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. region in OpenACC .kernels. construct" } */
    ;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. region in OpenACC .kernels. construct" } */
    for (y = 0; y < 10; y++)
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. region in OpenACC .kernels. construct" } */
    ;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. region in OpenACC .kernels. construct" } */
    for (y = 0; y < 10; y++)
      ;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. region in OpenACC .kernels. construct" } */
    for (y = 0; y < 10; y++)
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels
  /* { dg-optimized {'map\(force_tofrom:x \[len: [0-9]+\]\[implicit\]\)' optimized to 'map\(to:x \[len: [0-9]+\]\[implicit\]\)'} "" { target *-*-* } .-1 } */
  /* { dg-optimized {'map\(force_tofrom:y \[len: [0-9]+\]\[implicit\]\)' optimized to 'map\(to:y \[len: [0-9]+\]\[implicit\]\)'} "" { target *-*-* } .-2 } */
  /* { dg-optimized {'map\(force_tofrom:z \[len: [0-9]+\]\[implicit\]\)' optimized to 'map\(to:z \[len: [0-9]+\]\[implicit\]\)'} "" { target *-*-* } .-3 } */
  /* { dg-optimized {'map\(to:x \[len: [0-9]+\]\[implicit\]\)' further optimized to 'private\(x\)'} "" { target *-*-* } .-4 } */
  /* { dg-optimized {'map\(to:y \[len: [0-9]+\]\[implicit\]\)' further optimized to 'private\(y\)'} "" { target *-*-* } .-5 } */
  /* { dg-optimized {'map\(to:z \[len: [0-9]+\]\[implicit\]\)' further optimized to 'private\(z\)'} "TODO-kernels z not privatized?" { xfail *-*-* } .-6 } */
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .Graphite. part in OpenACC .kernels. region" } */
    for (y = 0; y < 10; y++)
      for (z = 0; z < 10; z++)
	;

  return 0;
}
