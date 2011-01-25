// { dg-do run }

extern "C" void abort ();

struct A
{
  inline ~A ();
  virtual void foo () {}
};

struct B : A
{
  virtual void foo () { abort(); }
};

static inline void middleman (A *a)
{
  a->foo ();
}

inline A::~A ()
{
  middleman (this);
}

int main ()
{
   B b;
   return 0;
}
