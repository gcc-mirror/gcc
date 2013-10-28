// { dg-options "-std=c++11" }

class X {
  template<typename T = int> friend void f(X) { }
  template<typename T> friend void g(X); // { dg-message "previously declared here" }
  template<typename T = int> friend void h(X); // { dg-error "function template friend" }
};

template<typename T = int> void g(X) // { dg-error "default template argument" }
{
}
