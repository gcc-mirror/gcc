// { dg-do assemble  }
// PRMS Id: 3069

void f(int&);			// { dg-error "" } referenced by error below
void g(const int& i) {
  f(i);				// { dg-error "" } discarding const
}
