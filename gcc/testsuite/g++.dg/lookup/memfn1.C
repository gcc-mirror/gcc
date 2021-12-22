// Verify we preserve the consistency of member function lookup outside of a
// complete-class context.
// { dg-do compile { target c++11 } }

template<class...>
struct A {
  template<class T> static void f();    // #1
  template<class T> static auto g() -> decltype(f<T>());
  template<class T> static void f(...); // #2
};

int main() {
  A<>::g<int>(); // OK, the later-declared #2 isn't considered when
		 // instantiating f<T>(), which would have otherwise
		 // led to ambiguity.
}
