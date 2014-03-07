// { dg-do compile { target c++11 } }

struct A {
    template <class U> using C = U;
};

// The particularity of the below struct is to have more than 7
// fields.  In this case, looking up a member here should exercise
// cp/search.c:lookup_field_1 in such a way that it finds it in the
// CLASSTYPE_SORTED_FIELDS of struct A7.
struct A7 {
  int f0;
  int f1;
  int f2;
  int f3;
  int f4;
  int f5;
  int f6;
  int f7;
  template <class U> using C = U;
};

template <class T>
struct B {
    typename T::template C<int> n;  //#0
};

// These should trigger the lookup
// of template C inside class A or
// A7, via #0.
B<A> b;
B<A7> c;
