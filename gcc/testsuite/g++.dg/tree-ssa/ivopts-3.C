// { dg-do compile }
// { dg-options "-O2 -fdump-tree-ivopts-details" }

class MinimalVec3
{
protected:
  double coords[3];

public:

  MinimalVec3( ) {
    for ( int i = 0; i < 3; ++i )
      coords[i] = 0.;
  }

  inline const double& operator[] ( int I ) const {
    return coords[I];
  }
};

class MinimalVector
{
protected:
  double *_pData;
  double stuff;

public:
  __attribute__((noinline)) explicit MinimalVector ( int length ) {
    _pData = new double[length];
    for (int i = 0; i < length; ++i) _pData[i] = 0.;
  }

  inline double& operator[] ( int I ) {
    return _pData[I];
  }

  inline const double& operator[] ( int I ) const {
    return _pData[I];
  }
};


int main ( int , char** ) {
    int w = ( 1 << 7 )+1;
    int wsqr = w*w;
    int wcub = w*w*w;

    MinimalVec3 * rows[9];
    for ( int i = 0; i < 9; ++i ) {
      rows[i] = new MinimalVec3[wcub];
    }

    MinimalVector img ( wcub ), res ( wcub );

    for ( int c = 0; c < 1000; ++c ) {

      for ( int i = 1; i < w-1; ++i )
        for ( int j = 0; j < 3; ++j ) {

          for ( int k = 1; k < w - 1; ++k )
            for ( int l = 0; l < 3; ++l ) {

              for ( int m = 1; m < w - 1; ++m )
                for ( int n = 0; n < 3; ++n )
                  res[i*wsqr + k*w + m] += img[( i + j - 1 ) *wsqr + ( k + l - 1 ) *w + m + n - 1] * rows[j*3 + l][i*wsqr + k*w + m][n];

            }
        }
    }
    return 0;
}

// Verify that on x86_64 and i?86 we use a single IV for the innermost loop

// { dg-final { scan-tree-dump "Selected IV set for loop \[0-9\]* at \[^ \]*:64, 3 avg niters, 1 IVs" "ivopts" { target x86_64-*-* i?86-*-* } } }
