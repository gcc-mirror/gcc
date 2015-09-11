// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR52: Non-static members, member selection and access checking

struct A 
{
  void foo(void);
};

struct B
{
private:
  void foo(void);
};

struct B1 : B {};
struct B2 : B {};

struct C
{ // { dg-message "declared" }
  void foo(void);
};

struct D : private C {};

struct X: A, B1, B2, D
{
public:
  void bar(void)
  {
    this->B::foo();  // { dg-error "" }
    this->C::foo();  // { dg-error "inaccessible|context" }
  }
};
