// PR c++/99214
// { dg-do compile { target c++20 } }

template <class T>
struct A {
  template <class U> static void f() requires requires { T::fail; };
};

int main() {
  A<int>::f<char>(); // { dg-error "no match" }
}

// { dg-message "In substitution of '\[^\r\n\]* \\\[with U = char\\\]'" "" { target *-*-* } 0 }
