// PRMS Id: 3069

void f(int&);			// ERROR - referenced by error below
void g(const int& i) {
  f(i);				// ERROR - discarding const
}
