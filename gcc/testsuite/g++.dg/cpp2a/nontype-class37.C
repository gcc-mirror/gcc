/* PR c++/94510 - nullptr_t implicitly cast to zero twice in std::array
   { dg-do compile { target c++20 } }
   { dg-options "-Wall" } */

struct A { char a[4]; };
template <A> struct B { };

constexpr const char c0{ };
constexpr const char c1{ 1 };

typedef B<A{ }>                     BA;
typedef B<A{ { } }>                 BA;
typedef B<A{ { 0 } }>               BA;
typedef B<A{ { c0 } }>              BA;
typedef B<A{ { 0, 0 } }>            BA;
typedef B<A{ { 0, 0, 0 } }>         BA;
typedef B<A{ { 0, 0, 0, 0 } }>      BA;
typedef B<A{ { c0, c0, c0 } }>      BA;
typedef B<A{ { c0, c0, c0, c0 } }>  BA;
typedef B<A{ "" }>                  BA;
typedef B<A{ "\0" }>                BA;
typedef B<A{ "\0\0" }>              BA;
typedef B<A{ "\0\0\0" }>            BA;

typedef B<A{ 1 }>                   BA1;
typedef B<A{ { 1 } }>               BA1;
typedef B<A{ { 1, 0 } }>            BA1;
typedef B<A{ { 1, 0, 0 } }>         BA1;
typedef B<A{ { 1, 0, 0, 0 } }>      BA1;
typedef B<A{ { c1 } }>              BA1;
typedef B<A{ { c1, c0 } }>          BA1;
typedef B<A{ { c1, c0, c0 } }>      BA1;
typedef B<A{ { c1, c0, c0, c0 } }>  BA1;
typedef B<A{ "\1" }>                BA1;
typedef B<A{ "\1\0" }>              BA1;
typedef B<A{ "\1\0\0" }>            BA1;

typedef B<A{ 0, 1 }>                BA01;
typedef B<A{ { 0, 1 } }>            BA01;
typedef B<A{ { 0, 1, 0 } }>         BA01;
typedef B<A{ { 0, 1, 0, 0 } }>      BA01;
typedef B<A{ { c0, c1 } }>          BA01;
typedef B<A{ { c0, c1, c0 } }>      BA01;
typedef B<A{ { c0, c1, c0, c0 } }>  BA01;
typedef B<A{ "\0\1" }>              BA01;
typedef B<A{ "\0\1\0" }>            BA01;


struct C { int a[4]; };
template <C> struct D { };

constexpr const int i0{ };

typedef D<C{ }>                     DC;
typedef D<C{ { } }>                 DC;
typedef D<C{ { 0 } }>               DC;
typedef D<C{ { 0, 0 } }>            DC;
typedef D<C{ { 0, 0, 0 } }>         DC;
typedef D<C{ { 0, 0, 0, 0 } }>      DC;
typedef D<C{ { i0 } }>              DC;
typedef D<C{ { i0, i0 } }>          DC;
typedef D<C{ { i0, i0, i0 } }>      DC;
typedef D<C{ { i0, i0, i0, i0 } }>  DC;


constexpr const int i1{ 1 };

typedef D<C{ 1 }>                   DC1;
typedef D<C{ { 1 } }>               DC1;
typedef D<C{ { 1, 0 } }>            DC1;
typedef D<C{ { 1, 0, 0 } }>         DC1;
typedef D<C{ { 1, 0, 0, 0 } }>      DC1;
typedef D<C{ { i1, i0, i0, i0 } }>  DC1;

typedef D<C{ 0, 1 }>                DC01;
typedef D<C{ { 0, 1 } }>            DC01;
typedef D<C{ { 0, 1, 0 } }>         DC01;
typedef D<C{ { 0, 1, 0, 0 } }>      DC01;
typedef D<C{ { 0, i1, 0, 0 } }>     DC01;
typedef D<C{ { i0, i1, i0, i0 } }>  DC01;   // { dg-bogus "conflicting declaration" "pr94567" { xfail *-*-* } }
