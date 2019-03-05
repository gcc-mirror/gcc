/* See also "../../gfortran.dg/goacc/loop-2-parallel-3.f95".  */

void par1 (void)
{
  int i, j;

#pragma acc parallel
  {
#pragma acc loop gang(5) // { dg-error "argument not permitted" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop gang(num:5) // { dg-error "argument not permitted" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop worker(5) // { dg-error "argument not permitted" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop worker(num:5) // { dg-error "argument not permitted" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop vector(5) // { dg-error "argument not permitted" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop vector(length:5) // { dg-error "argument not permitted" }
    for (i = 0; i < 10; i++)
      { }

   }
}

void p2 (void)
{
  int i, j;

#pragma acc parallel loop gang(5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc parallel loop gang(num:5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }

#pragma acc parallel loop worker(5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc parallel loop worker(num:5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }

#pragma acc parallel loop vector(5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc parallel loop vector(length:5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
}
