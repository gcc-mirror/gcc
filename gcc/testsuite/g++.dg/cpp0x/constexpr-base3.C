// PR c++/46526
// { dg-do run }
// { dg-options "-std=c++11" }

struct Base
{
  virtual int getid () = 0;
};

struct A : public Base
{
  virtual int getid () { return 1; }
};

struct B : public Base
{
  virtual int getid () { throw "here"; }
};

int
main ()
{
  A a;
  B b;
  Base& ar = a;
  ar.getid ();
}
