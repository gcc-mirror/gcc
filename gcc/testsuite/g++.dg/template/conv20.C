// Verify we check arity early before deduction without explicit
// template arguments.

template<class T>
struct A;

template<class T>
struct B : A<T> { };

template<class T> void f(A<T>&, int); // #1
template<class T> void f(B<T>&);      // #2

int main() {
  extern B<int> b;
  ::f(b); // OK, deduction for #1 short-circuited and B<int> not instantiated,
	  // which would have resulted in a hard error
}
