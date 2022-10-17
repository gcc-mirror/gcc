// PR c++/103852
// { dg-do compile { target c++17 } }
// { dg-options "" }

template <class T> struct b{};
template <class T, class T1 = b<T>>
struct s
{
    s(T);
};
s c(100);
template <class T, class T1 = b<T>>
using ss = s<T, T1>;	     // equivalent under proposed resolution of DR 1286
ss tt(1);   // { dg-warning "alias template deduction" "" { target c++17_only } }

template <class T, class T1 = T>
using ss2 = s<T, T1>;	     // different default arg makes it non-equivalent
ss2 tt2(1); // { dg-error "alias template deduction" "" { target c++17_only } }
