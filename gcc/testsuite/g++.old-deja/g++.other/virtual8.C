extern "C" int printf (const char*, ...);

struct A
{
  virtual void f () {
    printf ("%x\n", this);
  }
};

struct B : public A
{
};

struct C : public A
{
};

struct D : virtual public B, public C
{
};

int main ()
{
  D d;

  A* a1 = (A*) (B*) &d;
  A* a2 = (A*) (C*) &d;

  a1->f ();
  a2->f ();
}
