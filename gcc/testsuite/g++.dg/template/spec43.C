// PR c++/115716
// { dg-do compile { target c++20 } }
template <typename T> struct x {
  template <typename U> struct y { // { dg-note "original" }
    typedef T result2;
  };
};

template<>
template<typename U>
requires true
struct x<int>::y { // { dg-error "different constraints" }
  typedef double result2;
};

int main() {
  x<int>::y<int>::result2 xxx2;
}
