// PR c++/88061
// { dg-do compile { target { c++11 && named_sections } } }

template<class T>
struct A {
  [[gnu::section(".foo")]] static int var;
};

template<class T>
int A<T>::var = 42;

template struct A<int>;

// { dg-final { scan-assembler {\.(section|csect)[ \t]+"?\.foo} } }
