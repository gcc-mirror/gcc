// { dg-do compile }
// { dg-options "--std=c++98" }

struct B final {}; // { dg-warning "override controls" }

struct D : B {}; // { dg-error "cannot derive from 'final' base" }

struct E __final {};

struct F : E {}; // { dg-error "cannot derive from 'final' base" }

struct G
{
  virtual void f();
};

struct H : G
{
  void f() override; // { dg-warning "override controls" }
};

int main()
{
}
