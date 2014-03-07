// Negative reference variable tests.
// { dg-do compile { target c++11 } }

extern int *p;
constexpr int& ri = *p;		// { dg-error "p" }

extern constexpr int &er;	// { dg-error "not a definition" }
constexpr int& ri2 = er;	// { dg-error "er" }

void f(int j)
{
  constexpr int i = 42;
  constexpr int const& ri = i;	// { dg-error "" }

  constexpr int& rj = j;	// { dg-error "" }
}

