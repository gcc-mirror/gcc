/* Test the output of "-fopt-info-optimized-omp" for an OpenACC 'kernels'
   construct containing straight-line code.  */

/* { dg-additional-options "-fopt-info-optimized-omp" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

#pragma acc routine gang
extern int
f_g (int);

#pragma acc routine worker
extern int
f_w (int);

#pragma acc routine vector
extern int
f_v (int);

#pragma acc routine seq
extern int
f_s (int);

int
main ()
{
  int x, y, z;

#pragma acc kernels /* { dg-warning "region contains gang partitoned code but is not gang partitioned" } */
  {
    x = 0; /* { dg-message "optimized: beginning .gang-single. region in OpenACC .kernels. construct" } */
    y = x < 10;
    z = x++;
    ;

    y = 0;
    z = y < 10;
    x -= f_g (y++); /* { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" } */
    ;

    x = f_w (0); /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
    z = f_v (x < 10); /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
    y -= f_s (x++); /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    ;

    x = 0;
    y = x < 10;
    z = (x++);
    y = 0;
    x = y < 10;
    z += (y++);
    ;

    x = 0;
    y += f_s (x < 10); /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    x++;
    y = 0;
    y += f_v (y < 10); /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
    y++;
    z = 0;
    y += f_w (z < 10); /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
    z++;
    ;

    x = 0;
    y *= f_g ( /* { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" } */
	      f_w (x < 10) /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
	      + f_g (x < 10) /* { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" } */
	      );
    x++;
    y = 0;
    y *= y < 10;
    y++;
    z = 0;
    y *= z < 10;
    z++;
    ;
  }

  return 0;
}
