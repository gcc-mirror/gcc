// PR c++/70435
// { dg-do compile { target { c++11 && named_sections } } }

template<class T>
struct A {
  [[gnu::section(".foo")]] void fun() { }
};

template struct A<int>;

// { dg-final { scan-assembler {.(section|csect)[ \t]+.+foo} } }
