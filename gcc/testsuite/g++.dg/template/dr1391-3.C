// DR 1391

template <class T> struct Z {
  typedef typename T::x xx;
};
template <class T> typename Z<T>::xx f(void *, T);
template <class T> void f(int, T);
struct A {} a;
int main() {
  f(1, a); // If the implementation rules out the first overload
  // because of the invalid conversion from int to void*,
  // the error instantiating Z<A> will be avoided
}
