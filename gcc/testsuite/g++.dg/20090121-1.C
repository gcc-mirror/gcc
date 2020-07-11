// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-options "-flto -Wuninitialized -O2" }
class A
{
private:
  int y;

public:
  A () { int x; y = x + 1; } /* { dg-warning "'x' is used uninitialized" }  */
  int get_y () { return y; }
};

int foo()
{
  A a;
  return a.get_y ();
}

