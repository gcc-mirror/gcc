// { dg-do compile }
// { dg-require-effective-target vect_double }
// { dg-additional-options "-fexcess-precision=fast" }

#include <vector>
#include <complex>
#include <iostream>
#include <cstdlib>

using namespace std;

int
main(int argc, char** argv)
{
  if (argc < 3)
    {
      cout << "usage: size N" << endl;
      return -1;
    }

  const unsigned int size = atoi(argv[1]);
  const unsigned int N    = atoi(argv[2]);

  cout << "size = " << size << endl;
  cout << "N    = " << N    << endl;

  typedef complex<double> cx_double;

  vector< cx_double > A(size);
  vector< cx_double > B(size);
  vector< cx_double > C(size);

  cx_double* A_ptr = &A[0];
  cx_double* B_ptr = &B[0];
  cx_double* C_ptr = &C[0];

  for (unsigned int i=0; i<size; ++i)
    {
      A_ptr[i] = cx_double( (double(rand())/RAND_MAX), (double(rand())/RAND_MAX) );
      B_ptr[i] = cx_double( (double(rand())/RAND_MAX), (double(rand())/RAND_MAX) );
      C_ptr[i] = cx_double( double(0), double(0) );
    }

  for (unsigned int j=0; j<N; ++j)
    for (unsigned int i=0; i<size; ++i)
      C_ptr[i] = A_ptr[i] + B_ptr[i];

  cout << C_ptr[0] << endl;

  return 0;
}

// { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } }
