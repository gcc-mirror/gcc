/* See also "../../gfortran.dg/goacc/loop-2-serial-3.f95".  */

void f1 (void)
{
  int i, j;

#pragma acc serial
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

void f2 (void)
{
  int i, j;

#pragma acc serial loop gang(5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang(num:5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop worker(5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop worker(num:5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop vector(5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop vector(length:5) // { dg-error "argument not permitted" }
  for (i = 0; i < 10; i++)
    { }
}
