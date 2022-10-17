// PR c++/12672
// Verify we don't substitute explicit template arguments into
// candidate function templates when the arity of the function
// template disagrees with the arity of the call.

template<class T>
struct A { typedef typename T::type type; };

template<class T> void f(T); // arity 1
template<class T> void f(T, T, T); // arity 3

template<class T> typename A<T>::type f(T, T); // arity 2
template<class T, class U> typename A<T>::type f(U, U); // arity 2

struct B {
  template<class T> void f(T); // arity 1
  template<class T> void f(T, T, T); // arity 3

  template<class T> typename A<T>::type f(T, T); // arity 2
  template<class T, class U> typename A<T>::type f(U, U); // arity 2
};

int main() {
  // If overload resolution attempts deduction for any of the arity-2 function
  // templates, the substitution of explicit arguments into the template would
  // cause a hard error.
  f<int>(1);
  f<int>(1, 1, 1);

  B b;
  b.f<int>(1);
  b.f<int>(1, 1, 1);
}
