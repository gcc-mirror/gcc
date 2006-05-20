/* { dg-options "-O1" } */
struct A
{
  ERROR; /* { dg-error "ERROR" } */
  ~A();
};

struct B
{
  virtual ~B();
};

struct C : B, A {};

struct D : C {};

D d;
