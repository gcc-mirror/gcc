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
   return f;
}

const A<float> &g3()
{
   float f[] = {1.1f, 2.3f};
   return f; // { dg-warning "returning reference to temporary" }
}

A<float> &g4()
{
   float f[] = {1.1f, 2.3f};
   return f; // { dg-error "cannot bind non-const lvalue ref" }
}

struct B
{
   B (int (&v)[10]);
   B();
};

B g2()
{
   int c[10];
   return c;
}

