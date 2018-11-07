// PR c++/87095
// { dg-do run }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

struct A
{
  virtual ~A () {}
};

struct B : virtual A {};

struct C : B {};

int
main ()
{
  C c;
  return 0;
}
