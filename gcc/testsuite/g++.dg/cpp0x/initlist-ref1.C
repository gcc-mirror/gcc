// PR c++/113141
// { dg-do compile { target c++11 } }

struct ConvToRef {
  operator int&();
};

struct A { int& r; };

void f(A);

int main() {
  ConvToRef c;
  A a{{c}};
  f({{c}});
}
