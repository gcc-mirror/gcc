// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<class T>
class A {
 public:
  template<int I, class S>
    requires (I > 0)
  friend int f1(const A<S>&);

  template<int I, class S>
  friend int f2(const A<S>&) requires (I > 0);

 private:
  int x = 2;
};

template<int I, class S>
  requires (I > 0)
int f1(const A<S>& a)  { 
  return a.x;
} 

template<int I, class S>
int f2(const A<S>& a) requires (I > 0) { 
  return a.x;
} 

class B {
 public:
  template<int I>
    requires (I > 0)
  friend int f3(const B&);

  template<int I>
  friend int f4(const B&) requires (I > 0);

 private:
  int x = 2;
};

template<int I>
  requires (I > 0)
int f3(const B& a) {
  return a.x;
}

template<int I>
int f4(const B& a) requires (I > 0) {
  return a.x;
}

int main() { 
  A<double> a;
  f1<2>(a);
  f2<2>(a);

  B b;
  f3<2>(b);
  f4<2>(b);

  return 0;
}
