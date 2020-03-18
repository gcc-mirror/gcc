// PR c++/94057 - template keyword in a typename-specifier.
// { dg-do compile { target c++11 } }

template<typename T> struct A {
  template<typename U>
  struct W { };
};

void
g ()
{
  // class-key nested-name-specifier template[opt] simple-template-id
  struct A<int>::W<int> w;
  struct A<int>::template W<int> w2;
}
