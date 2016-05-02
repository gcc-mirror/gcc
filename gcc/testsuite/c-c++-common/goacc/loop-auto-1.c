
void Foo ()
{
  
#pragma acc parallel num_gangs(10) num_workers(32) vector_length(32)
  {
#pragma acc loop vector
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop seq
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++) {}
      }

#pragma acc loop worker
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop vector
	    for (int kx = 0; kx < 10; kx++) {}
	  }
      }

#pragma acc loop gang
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }

#pragma acc loop worker
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }

#pragma acc loop vector
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	    for (int kx = 0; kx < 10; kx++) {}
	  }

#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop vector
	    for (int kx = 0; kx < 10; kx++) {}
	  }

      }
    
#pragma acc loop auto
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }
      }
  }
}

#pragma acc routine gang
void Gang (void)
{
#pragma acc loop vector
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop seq
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++) {}
      }

#pragma acc loop worker
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop vector
	    for (int kx = 0; kx < 10; kx++) {}
	  }
      }

#pragma acc loop gang
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }

#pragma acc loop worker
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }

#pragma acc loop vector
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	    for (int kx = 0; kx < 10; kx++) {}
	  }

#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop vector
	    for (int kx = 0; kx < 10; kx++) {}
	  }

      }
    
#pragma acc loop auto
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }
      }
}

#pragma acc routine worker
void Worker (void)
{
#pragma acc loop vector
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop seq
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++) {}
      }

#pragma acc loop worker
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop vector
	    for (int kx = 0; kx < 10; kx++) {}
	  }
      }

#pragma acc loop auto
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto
	for (int jx = 0; jx < 10; jx++) {}
      }

#pragma acc loop auto
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++)
	  {
#pragma acc loop auto
	    for (int kx = 0; kx < 10; kx++) {}
	  }
      }
}

#pragma acc routine vector
void Vector (void)
{
#pragma acc loop vector
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop seq
	for (int jx = 0; jx < 10; jx++) {}

#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++) {}
      }

#pragma acc loop auto
    for (int ix = 0; ix < 10; ix++) {}

#pragma acc loop auto
    for (int ix = 0; ix < 10; ix++)
      {
#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
	for (int jx = 0; jx < 10; jx++) {}
      }
}

#pragma acc routine seq
void Seq (void)
{
#pragma acc loop auto /* { dg-warning "insufficient partitioning" } */
    for (int ix = 0; ix < 10; ix++) {}
}
