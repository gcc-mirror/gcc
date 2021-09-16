// { dg-do compile { target c++11 } }
// { dg-additional-options "-fpermissive" }

struct A {
  A(int*, int);
};

void f(A);

int main() {
  const int n = 0;
  f({&n, 42}); // { dg-warning "invalid conversion from 'const int\\*' to 'int\\*'" }
}
