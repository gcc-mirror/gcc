template <typename T>
void f(int, T (*)() = 0);	// { dg-message "note" }

void g() {
  typedef int A[2];
  f<A>(0); // { dg-error "" }
  // { dg-error "returning an array" "candidate explanation" { target *-*-* } 2 }
  typedef void F();
  f<F>(0); // { dg-error "" }
  // { dg-error "returning a function" "candidate explanation" { target *-*-* } 2 }
}
