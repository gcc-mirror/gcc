// { dg-do compile { target c++11 } }

class X {
  template<typename T = int> friend void f(X) { } // OK
  template<typename T> friend void g(X); // { dg-message "previously declared here" }
  template<typename T = int> friend void h(X); // { dg-error "template friend" }
};

template<typename T = int> void g(X) // { dg-error "default template argument" }
{
}
