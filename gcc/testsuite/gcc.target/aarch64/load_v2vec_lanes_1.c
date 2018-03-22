/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long long v2di __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

v2di
construct_lanedi (long long *y)
{
  v2di x = { y[0], y[1] };
  return x;
}

v2df
construct_lanedf (double *y)
{
  v2df x = { y[0], y[1] };
  return x;
}

/* We can use the load_pair_lanes<mode> pattern to vec_concat two DI/DF
   values from consecutive memory into a 2-element vector by using
   a Q-reg LDR.  */

/* { dg-final { scan-assembler-times "ldr\tq\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-not "ins\t" } } */
