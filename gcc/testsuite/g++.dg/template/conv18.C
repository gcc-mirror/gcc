// PR c++/104622
// { dg-additional-options "-fpermissive" }

template<class T>
struct type_identity {
  typedef T type;
};

template<class T> void f(typename type_identity<T>::type*, T, int*);

int main() {
  const int p = 0;
  f(&p, 0, 0); // { dg-warning "invalid conversion" }
}
