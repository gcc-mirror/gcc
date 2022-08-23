// PR c++/103057
// { dg-do compile { target c++11 } }

template <class T> struct A { };
template <class T> struct B { using type = A<T>; };
template <class T> struct C {
  using type = typename T::foo;	// { dg-error "int" }
};
template <class T> using L = B<void>;

template <class T>
typename L<typename C<T>::type>::type
f(T) { };

int main()
{
  f(42);			// { dg-error "no match" }
}
