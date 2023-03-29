// PR c++/96745
// { dg-do compile { target c++20 } }

template<class T>
struct A { // { dg-error "destructor for 'A<int>' is ambiguous" }
  ~A() requires true;
  ~A() requires (!!true);
};

A<int> a;

template<class T>
struct B { // { dg-error "no viable destructor for 'B<int>'" }
  ~B() requires false;
  ~B() requires (!!false);
};

B<int> b;
