// Check that indirect calls to thunks do not lead to errors.
// { dg-do run }
// { dg-options "-O" }

extern "C" void abort ();

class A
{
public:
  virtual void foo () {abort();}
};

class B : public A
{
public:
  int z;
  virtual void foo () {abort();}
};

class C : public A
{
public:
  void *a[32];
  unsigned long b;
  long c[32];

  virtual void foo () {abort();}
};

class D : public C, public B
{
public:
  D () : C(), B()
  {
    int i;
    for (i = 0; i < 32; i++)
      {
	a[i] = (void *) 0;
	c[i] = 0;
      }
    b = 0xaaaa;
  }

  virtual void foo ();
};

inline void D::foo()
{
  if (b != 0xaaaa)
    abort();
}

static inline void bar (B &b)
{

  b.foo ();
}

int main()
{
  int i;
  D d;

  for (i = 0; i < 5000; i++)
    bar (d);
  return 0;
}
