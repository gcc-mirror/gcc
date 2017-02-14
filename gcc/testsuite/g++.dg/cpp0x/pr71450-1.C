// PR c++/71450
// { dg-do compile { target c++11 } }

struct A { A operator+ (A a) { return a; } };

template <class T>
void foo (T t)
{ 
  auto x = t + x;	// { dg-error "use of 'x' before deduction of 'auto'" }
}

int
main ()
{ 
  foo (A ());
}
