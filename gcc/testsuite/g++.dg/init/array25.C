// related to PR c++/38233
// test for value-init of a member array
// { dg-do run }

struct elt 
{
  virtual void f();
  char c;
};

void elt::f() { }

struct foo {
  elt buffer[500];
  foo() ;
  bool check () const;
};

foo::foo ()
  : buffer()
{}

bool foo::check () const
{
  for (unsigned ix = sizeof (buffer)/ sizeof (buffer[0]); ix--;)
    if (buffer[ix].c)
      return false;
  return true;
}

inline void *operator new (__SIZE_TYPE__ size, void *p)
{
  return p;
}

char heap[sizeof(elt[500])];

int main ()
{
  for (unsigned ix = sizeof (heap); ix--;)
    heap[ix] = ix;

  foo *f = new (heap) foo ();
  if (!f->check ())
    return 3;
  return 0;
}

  
