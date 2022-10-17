// { dg-do compile { target c++20 } }

template<class T>
void f() {
  g<int>(T{}); // { dg-error "argument-dependent lookup" }
	       // { dg-bogus "no match" "" { target *-*-* } .-1 }
}

template<class T>
void g(int);   // { dg-message "declared here, later" }

template void f<int>();
