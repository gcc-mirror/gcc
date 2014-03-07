// PR c++/56373
// { dg-do compile { target c++11 } }
// { dg-options "-Wzero-as-null-pointer-constant" }

struct shared_ptr
{
  shared_ptr(decltype(nullptr));
};

void f()
{
  shared_ptr a = 0;  // { dg-warning "zero as null pointer" }
  shared_ptr b(0);   // { dg-warning "zero as null pointer" }
  shared_ptr c{0};   // { dg-warning "zero as null pointer" }
}
