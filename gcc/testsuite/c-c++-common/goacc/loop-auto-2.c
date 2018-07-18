
// Tile parititioning

void Ok ()
{
#pragma acc parallel num_gangs (10) num_workers(32) vector_length(32)
  {
    
#pragma acc loop tile(*) gang vector
    for (int ix = 0; ix < 10; ix++)
      {
      }

#pragma acc loop tile(*)
    for (int ix = 0; ix < 10; ix++)
      {
      }

#pragma acc loop tile(*) gang
    for (int ix = 0; ix < 10; ix++)
      {
	#pragma acc loop vector
	for (int jx = 0; jx < 10; jx++)
	  ;
      }

#pragma acc loop tile(*)
    for (int ix = 0; ix < 10; ix++)
      {
	#pragma acc loop vector
	for (int jx = 0; jx < 10; jx++)
	  ;
      }

#pragma acc loop gang
    for (int jx = 0; jx < 10; jx++)
      {
#pragma acc loop tile(*) vector
	for (int ix = 0; ix < 10; ix++)
	  {
	  }

#pragma acc loop tile(*)
	for (int ix = 0; ix < 10; ix++)
	  {
	  }
      }

#pragma acc loop tile(*) worker
    for (int ix = 0; ix < 10; ix++)
      {
	#pragma acc loop vector
	for (int jx = 0; jx < 10; jx++)
	  ;
      }
  }
}

void Bad ()
{
#pragma acc parallel num_gangs (10) num_workers(32) vector_length(32)
  {
    
#pragma acc loop tile(*) gang vector /* { dg-message "containing loop" } */
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop vector /* { dg-error "uses same" } */
	for (int jx = 0; jx < 10; jx++)
	  ;
      }

#pragma acc loop tile(*) gang vector
    for (int ix = 0; ix < 10; ix++)
      {
	#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++)
	  ;
      }

#pragma acc loop tile(*) auto /* { dg-warning "insufficient partitioning" } */
    for (int ix = 0; ix < 10; ix++)
      {
	#pragma acc loop worker
	for (int jx = 0; jx < 10; jx++)
	  ;
      }

#pragma acc loop worker /* { dg-message "containing loop" } */
    for (int jx = 0; jx < 10; jx++)
      {
#pragma acc loop tile(*) gang vector /* { dg-error "incorrectly nested" } */
	for (int ix = 0; ix < 10; ix++)
	  {
	  }

#pragma acc loop tile(*) vector /* { dg-warning "insufficient partitioning" } */
	for (int ix = 0; ix < 10; ix++)
	  {
	  }

#pragma acc loop tile(*) /* { dg-warning "insufficient partitioning" } */
	for (int ix = 0; ix < 10; ix++)
	  {
	  }
      }
  }
}
