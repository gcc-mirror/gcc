// PR c++/31337
// { dg-options "" }

struct A
{
  int i[0];
  A();
  A(const A&);
  ~A();
};

void foo()
{
  A a = ({ A(); });
}
