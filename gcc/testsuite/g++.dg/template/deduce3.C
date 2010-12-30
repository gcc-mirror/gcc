template <typename T>
void f(int, T (*)() = 0);	// { dg-message "note" }

void g() {
  typedef int A[2];
  f<A>(0); // { dg-error "" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 6 }
  typedef void F();
  f<F>(0); // { dg-error "" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 9 }
}
