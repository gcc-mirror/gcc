/* See also "../../gfortran.dg/goacc/loop-2-kernels.f95".  */

void K(void)
{
  int i, j;

#pragma acc kernels
  {
#pragma acc loop auto
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(num:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:*)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang
    for (i = 0; i < 10; i++)
      {
#pragma acc loop vector 
	for (j = 0; j < 10; j++)
	  { }
#pragma acc loop worker 
	for (j = 0; j < 10; j++)
	  { }
#pragma acc loop gang // { dg-error "inner loop uses same" }
	for (j = 0; j < 10; j++)
	  { }
      }
#pragma acc loop seq gang // { dg-error "'seq' overrides" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop worker
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker(5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker(num:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker
    for (i = 0; i < 10; i++)
      {
#pragma acc loop vector 
	for (j = 0; j < 10; j++)
	  { }
#pragma acc loop worker // { dg-error "inner loop uses same" }
	for (j = 0; j < 10; j++)
	  { }
#pragma acc loop gang
	for (j = 0; j < 10; j++)
	  { }
      }
#pragma acc loop seq worker // { dg-error "'seq' overrides" }
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang worker
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop vector
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector(5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector(length:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector
    for (i = 0; i < 10; i++)
      {
#pragma acc loop vector // { dg-error "inner loop uses same" }
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop worker
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop gang
	for (j = 1; j < 10; j++)
	  { }
      }
#pragma acc loop seq vector // { dg-error "'seq' overrides" }
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang vector
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker vector
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop auto
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop seq auto // { dg-error "'seq' overrides" }
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang auto // { dg-error "'auto' conflicts" }
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker auto // { dg-error "'auto' conflicts" }
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector auto // { dg-error "'auto' conflicts" }
    for (i = 0; i < 10; i++)
      { }
  }

#pragma acc kernels loop auto
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang(5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang(num:5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang(static:5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang(static:*)
  for (i = 0; i < 10; i++)
    { }

#pragma acc kernels loop worker
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop worker(5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop worker(num:5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop seq worker // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang worker
  for (i = 0; i < 10; i++)
    { }

#pragma acc kernels loop vector
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop vector(5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop vector(length:5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop seq vector // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang vector
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop worker vector
  for (i = 0; i < 10; i++)
    { }

#pragma acc kernels loop auto
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop seq auto // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop gang auto // { dg-error "'auto' conflicts" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop worker auto // { dg-error "'auto' conflicts" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc kernels loop vector auto // { dg-error "'auto' conflicts" }
  for (i = 0; i < 10; i++)
    { }
}
