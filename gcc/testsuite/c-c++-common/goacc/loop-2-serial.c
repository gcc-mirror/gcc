/* See also "../../gfortran.dg/goacc/loop-2-serial.f95".  */

void S(void)
{
  int i, j;

#pragma acc serial
  {
#pragma acc loop auto
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:*)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang // { dg-message "containing loop" }
    for (i = 0; i < 10; i++)
      {
#pragma acc loop vector
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop worker 
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop gang // { dg-error "inner loop uses same" }
	for (j = 1; j < 10; j++)
	  { }
      }
#pragma acc loop seq gang // { dg-error "'seq' overrides" }
    for (i = 0; i < 10; i++)
      { }

#pragma acc loop worker
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker // { dg-message "containing loop" 2 }
    for (i = 0; i < 10; i++)
      {
#pragma acc loop vector 
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop worker // { dg-error "inner loop uses same" }
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop gang // { dg-error "incorrectly nested" }
	for (j = 1; j < 10; j++)
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
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector // { dg-message "containing loop" 3 }
    for (i = 0; i < 10; i++)
      {
#pragma acc loop vector // { dg-error "inner loop uses same" }
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop worker // { dg-error "incorrectly nested" }
	for (j = 1; j < 10; j++)
	  { }
#pragma acc loop gang // { dg-error "incorrectly nested" }
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

#pragma acc serial loop auto
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang(static:5)
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang(static:*)
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop seq gang // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop worker
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop seq worker // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang worker
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop vector
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop seq vector // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang vector
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop worker vector
  for (i = 0; i < 10; i++)
    { }

#pragma acc serial loop auto
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop seq auto // { dg-error "'seq' overrides" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop gang auto // { dg-error "'auto' conflicts" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop worker auto // { dg-error "'auto' conflicts" }
  for (i = 0; i < 10; i++)
    { }
#pragma acc serial loop vector auto // { dg-error "'auto' conflicts" }
  for (i = 0; i < 10; i++)
    { }
}
