// PR c++/38597
// { dg-do compile { target c++11 } }

template<class T, class U>
auto f(T,U) -> decltype(T() + U())
{ return T() + U(); }

template<class T> void g(T){}	// { dg-message "note" }

int main() { g(f); }		// { dg-error "no matching function" }
// { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 10 }

