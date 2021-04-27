// PR c++/88580
// { dg-do compile { target c++11 } }

template <class... Bases>
struct Derived : Bases... {
  template <class... Ts>
  Derived(Ts... args) : Bases(args, args..., args)... { }
};

struct A { };
struct B { };
struct C { };

struct Base1 { Base1(A, A, B, C, A); };
struct Base2 { Base2(B, A, B, C, B); };
struct Base3 { Base3(C, A, B, C, C); };

Derived<Base1, Base2, Base3> d(A{}, B{}, C{});
