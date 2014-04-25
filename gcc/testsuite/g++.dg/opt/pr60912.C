// { dg-do run }
// { dg-options "-O -fno-inline -fipa-pta" }

struct IFoo
{
  virtual void Foo () = 0;
};

struct Bar:IFoo
{
  void Foo () {}
};

int main ()
{
  (new Bar ())->Foo ();
  return 0;
}
