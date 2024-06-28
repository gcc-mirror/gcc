// PR c++/106760
// { dg-additional-options "-fpermissive" }

struct S {
  template<class> int f();
  template<class> int g(...);
};

int main() {
  const S s;
  s.f<void>(); // { dg-warning "discards qualifiers" }
  s.g<void>(); // { dg-warning "discards qualifiers" }
}
