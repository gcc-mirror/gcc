/* { dg-do run } */

extern "C" void abort ();

struct B *b;

struct B
{
  virtual void f () { }
  ~B() { b->f(); }
};

struct D : public B
{
  virtual void f () { abort (); }
};

int main ()
{
  D d;
  b = &d;
  return 0;
}
