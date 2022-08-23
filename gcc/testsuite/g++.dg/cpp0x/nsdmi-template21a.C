// PR c++/104225
// { dg-do compile { target c++11 } }

struct A { private: ~A(); };

template<class> struct B { A a = A(); }; // { dg-error "private" }

int main() {
  new B<int>; // { dg-error "deleted" }
}
