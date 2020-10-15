/* PR c/97413 - bogus error on function declaration with many VLA arguments:
   wrong number of arguments specified for 'access' attribute
   { dg-do compile }
   { dg-options "-Wall" } */

extern int n;

void f1 (int[n]);
void f2 (int[n], int[n]);
void f3 (int[n], int[n], int[n]);
void f4 (int[n], int[n], int[n], int[n]);
void f5 (int[n], int[n], int[n], int[n], int[n]);
void f6 (int[n], int[n], int[n], int[n], int[n], int[n]);
void f7 (int[n], int[n], int[n], int[n], int[n], int[n], int[n]);
void f8 (int[n], int[n], int[n], int[n], int[n], int[n], int[n], int[n]);
void f9 (int[n], int[n], int[n], int[n], int[n], int[n], int[n], int[n],
	 int[n]);
void f10 (int[n], int[n], int[n], int[n], int[n], int[n], int[n], int[n],
	  int[n], int[n]);


void f1 (int[n]);
void f2 (int[n], int[n]);
void f3 (int[n], int[n], int[n]);
void f4 (int[n], int[n], int[n], int[n]);
void f5 (int[n], int[n], int[n], int[n], int[n]);
void f6 (int[n], int[n], int[n], int[n], int[n], int[n]);
void f7 (int[n], int[n], int[n], int[n], int[n], int[n], int[n]);
void f8 (int[n], int[n], int[n], int[n], int[n], int[n], int[n], int[n]);
void f9 (int[n], int[n], int[n], int[n], int[n], int[n], int[n], int[n],
	 int[n]);
void f10 (int[n], int[n], int[n], int[n], int[n], int[n], int[n], int[n],
	  int[n], int[n]);


void g (int n)
{
  typedef int A[n];

  void g1 (A);
  void g2 (A, A);
  void g3 (A, A, A);
  void g4 (A, A, A, A);
  void g5 (A, A, A, A, A);
  void g6 (A, A, A, A, A, A);
  void g7 (A, A, A, A, A, A, A);
  void g8 (A, A, A, A, A, A, A, A);
  void g9 (A, A, A, A, A, A, A, A, A);
  void g10 (A, A, A, A, A, A, A, A, A, A);

  void g1 (A);
  void g2 (A, A);
  void g3 (A, A, A);
  void g4 (A, A, A, A);
  void g5 (A, A, A, A, A);
  void g6 (A, A, A, A, A, A);
  void g7 (A, A, A, A, A, A, A);
  void g8 (A, A, A, A, A, A, A, A);
  void g9 (A, A, A, A, A, A, A, A, A);
  void g10 (A, A, A, A, A, A, A, A, A, A);


  typedef int B[n][n + 1][n + 2][n + 3][n + 4][n + 5][n + 7];

  void h1 (B);
  void h2 (B, B);
  void h3 (B, B, B);
  void h4 (B, B, B, B);
  void h5 (B, B, B, B, B);
  void h6 (B, B, B, B, B, B);
  void h7 (B, B, B, B, B, B, B);
  void h8 (B, B, B, B, B, B, B, B);
  void h9 (B, B, B, B, B, B, B, B, B);
  void h10 (B, B, B, B, B, B, B, B, B, B);

  void h1 (B);
  void h2 (B, B);
  void h3 (B, B, B);
  void h4 (B, B, B, B);
  void h5 (B, B, B, B, B);
  void h6 (B, B, B, B, B, B);
  void h7 (B, B, B, B, B, B, B);
  void h8 (B, B, B, B, B, B, B, B);
  void h9 (B, B, B, B, B, B, B, B, B);
  void h10 (B, B, B, B, B, B, B, B, B, B);
}
