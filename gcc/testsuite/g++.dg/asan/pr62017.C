// PR c++/62017
// { dg-do run }

struct A
{
  int x;
  virtual ~A () {}
};
struct B : public virtual A {};
struct C : public virtual A {};
struct D : public B, virtual public C {};

int
main ()
{
  D d;
}
