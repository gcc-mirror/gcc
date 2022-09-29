// PR c++/41426

template <typename _T>
struct A
{
   template <int _N>
       A(_T (&V)[_N]);
   A();
};

A<float> g1()
{
   float f[] = {1.1f, 2.3f};
   return f; // { dg-error "cannot bind non-const" "" { target c++23 } }
}

const A<float> &g3()
{
   float f[] = {1.1f, 2.3f};
   return f; // { dg-warning "returning reference to temporary" "" { target c++20_down } }
// { dg-error "non-const lvalue|invalid user-defined conversion" "" { target c++23 } .-1 }
}

A<float> &g4()
{
   float f[] = {1.1f, 2.3f};
   return f; // { dg-error "cannot bind non-const lvalue ref|invalid user-defined conversion" }
}

struct B
{
   B (int (&v)[10]);
   B();
};

B g2()
{
   int c[10];
   return c; // { dg-error "non-const lvalue" "" { target c++23 } }
}
