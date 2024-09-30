// PR c++/115716
// { dg-do compile }
template <typename T> struct x {
  template <typename U> struct y { // { dg-note "used 1 template parameter" }
    typedef T result2;
  };
};

template<>
template<typename U, typename>
struct x<int>::y { // { dg-error "redeclared with 2 template parameters" }
  typedef double result2;
};

int main() {
  x<int>::y<int>::result2 xxx2;
}
