int main ()
{
#pragma acc parallel
  {
#pragma acc loop tile (*,*)
    for (int ix = 0; ix < 30; ix++) /* { dg-error "not enough" } */
      ;

#pragma acc loop tile (*,*)
    for (int ix = 0; ix < 30; ix++)
      for (int jx = 0; jx < ix; jx++) /* { dg-error "condition expression" } */
	;
    
#pragma acc loop tile (*)
    for (int ix = 0; ix < 30; ix++)
      for (int jx = 0; jx < ix; jx++) /* OK */
	;
    
  }
  return 0;
}
