/* Verify the accuracy of the line number associated with combined
   constructs.  */
/* See also "../../gfortran.dg/goacc/combined-directives-3.f90".  */

int
main ()
{
  int x, y, z;

#pragma acc parallel loop seq auto /* { dg-error "'seq' overrides other OpenACC loop specifiers" } */
  for (x = 0; x < 10; x++)
#pragma acc loop
    for (y = 0; y < 10; y++)
      ;

#pragma acc parallel loop gang auto /* { dg-error "'auto' conflicts with other OpenACC loop specifiers" } */
  for (x = 0; x < 10; x++)
#pragma acc loop worker auto /* { dg-error "'auto' conflicts with other OpenACC loop specifiers" } */
    for (y = 0; y < 10; y++)
#pragma acc loop vector
      for (z = 0; z < 10; z++)
	;


#pragma acc serial loop seq auto /* { dg-error "'seq' overrides other OpenACC loop specifiers" } */
  for (x = 0; x < 10; x++)
#pragma acc loop
    for (y = 0; y < 10; y++)
      ;

#pragma acc serial loop gang auto /* { dg-error "'auto' conflicts with other OpenACC loop specifiers" } */
  for (x = 0; x < 10; x++)
#pragma acc loop worker auto /* { dg-error "'auto' conflicts with other OpenACC loop specifiers" } */
    for (y = 0; y < 10; y++)
#pragma acc loop vector
      for (z = 0; z < 10; z++)
	;


  return 0;
}
