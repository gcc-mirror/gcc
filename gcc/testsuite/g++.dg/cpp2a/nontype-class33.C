// PR c++/90938 - Initializing array with {1} works, but not {0}
// { dg-do compile { target c++2a } }
// { dg-options "-Wall" }

struct A { int i; };
struct B { A a[2]; };

static const constexpr A a0 = { 0 };
static const constexpr A a_ = { };

template <B> struct X { };

typedef X<B{ }>             XB;
typedef X<B{{A{ }}}>        XB;
typedef X<B{{A{ 0 }}}>      XB;
typedef X<B{{a_}}>          XB;
typedef X<B{{a0}}>          XB;
typedef X<B{{a_, A{ }}}>    XB;
typedef X<B{{a_, A{ 0 }}}>  XB;
typedef X<B{{a_, a_}}>      XB;
typedef X<B{{a_, a0}}>      XB;


struct C { constexpr C () = default; };
struct D { C c[2]; };

static const constexpr C c_ = { };

template <D> struct Y { };

typedef Y<D{ }>             YD;
typedef Y<D{C { }}>         YD;
typedef Y<D{{c_}}>          YD;
typedef Y<D{C{ }, C{ }}>    YD;
typedef Y<D{C{ }, c_}>      YD;
typedef Y<D{{c_, c_}}>      YD;
