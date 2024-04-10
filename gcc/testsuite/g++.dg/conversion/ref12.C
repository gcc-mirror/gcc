// PR c++/113141

struct Matrix { };

struct TPoint3 { private: operator const Matrix(); };

void f(Matrix&);

int main() {
  TPoint3 X;
  Matrix& m = (Matrix &)X;	// { dg-warning "does not use" }
  f((Matrix &)X);		// { dg-warning "does not use" }
}

struct A { private: operator const int&(); } a;
int &r = (int&)a;		// { dg-warning "does not use" }

struct B { B(int); };
int i;
B &br = (B&)i;			// { dg-warning "does not use" }
