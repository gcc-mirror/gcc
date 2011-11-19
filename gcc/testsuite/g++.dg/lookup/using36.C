// PR c++/25994
// { dg-do run }

struct B1
{
  void f (char) {}
  void f (double) { __builtin_abort(); }
};

struct B2
{
  void f (double) { __builtin_abort(); }
  void f (int) {}
};

struct D : public B1, public B2
{
  using B1::f;
  using B2::f;
  void g ()
  {
    f ('a');           // should call B1::f(char)
    f (33);            // should call B2::f(int)
  }
};

int main()
{
  D d;
  d.g();
}
