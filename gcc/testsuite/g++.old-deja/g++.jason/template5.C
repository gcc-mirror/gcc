// { dg-do assemble  }
// Bug: g++ fails to compare integer constants properly.

template <int X, int Y>
struct Matrix {
   int base [X] [Y];
};

template <int M,int H,int N>
Matrix<M,N>& Mul(Matrix<M,N>& Q,Matrix<M,H>& A,Matrix<H,N>& B) {
  for(int i=0;i<M;i++) {
    for(int j=0;j<N;j++) {
      Q.base[i][j]=0;
      for(int k=0;k<H;k++) {
	Q.base[i][j]+=A.base[i][k]*B.base[k][j];
      }
    }
  }
  return Q;
}

void f ()
{
   Matrix<2, 3> q;
   Matrix<2, 4> a;
   Matrix<4, 3> b;
   q = Mul (q, a, b);
}
