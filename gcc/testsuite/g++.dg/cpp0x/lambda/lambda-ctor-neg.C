// { dg-do compile { target c++11 } }

void f()
{
  int i;
  auto lam = [i]{};		// { dg-message "" }
  decltype(lam) lam2 = { 1 };	// { dg-error "" "not an aggregate" }
  decltype(lam) lam3;		// { dg-error "" "deleted default ctor" }
  lam3 = lam;			// { dg-error "" "deleted assignment op" }
}

template <class T>
void g(T i)
{
  auto lam = [i]{};		// { dg-message "" }
  decltype(lam) lam2 = { 1 };	// { dg-error "" "not an aggregate" }
  decltype(lam) lam3;		// { dg-error "" "deleted default ctor" }
  lam3 = lam;			// { dg-error "" "deleted assignment op" }
}

int main()
{
  f();
  g(1);
}
