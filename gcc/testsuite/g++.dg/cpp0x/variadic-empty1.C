// PR c++/85093
// { dg-do compile { target c++11 } }

template<class V> class A {};

template<class V, class... G> class B {
  typedef A<V,G...> AB;		// { dg-error "arguments" }
  AB ab;
};

int main() {
  B<int,double> b;  
}
