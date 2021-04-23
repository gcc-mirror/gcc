/* Diagnostics about potentially suboptimal choices related to OpenACC
   parallelism.

   { dg-additional-options "-Wopenacc-parallelism" }
*/


//TODO 'kernels'

//TODO 'serial'

//TODO 'routine'

//TODO Fortran


static void f1 ()
{
  int ary[10];


#pragma acc parallel num_gangs (1)
  /* { dg-warning "region contains gang partitioned code but is not gang partitioned" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop gang
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel num_workers (1)
  /* { dg-warning "region contains worker partitioned code but is not worker partitioned" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop worker
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel vector_length (1)
  /* { dg-warning "region contains vector partitioned code but is not vector partitioned" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop vector
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }
}


static void f2 ()
{
  int ary[10];


#pragma acc parallel num_gangs (8)
  /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop worker
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel num_gangs (8)
  /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop vector
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel num_gangs (8)
  /* { dg-warning "region is gang partitioned but does not contain gang partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop worker vector
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel num_workers (8)
  /* { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop gang
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel num_workers (8)
  /* { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop vector
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel num_workers (8)
  /* { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop gang vector
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel vector_length (8)
  /* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop gang
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel vector_length (8)
  /* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop worker
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel vector_length (8)
  /* { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-1 } */
  {
    #pragma acc loop gang worker
    for (int i = 0; i < 10; i++)
      ary[i] = i;
  }
}
