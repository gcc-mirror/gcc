/* Verify that ipa-cp will not get confused by placement new constructing an
   object within another one when looking for dynamic type change .  */
/* { dg-do run } */
/* { dg-options "-O3 -Wno-attributes"  } */

extern "C" void abort (void);
namespace std {
  typedef __SIZE_TYPE__ size_t;
}
inline void* __attribute__ ((always_inline))
operator new(std::size_t, void* __p) throw()
{
  return __p;
}

class A
{
public:
  char data[256];
  A();
  virtual int foo (int i);
};

class B : public A
{
public:
  virtual int foo (int i);
};

class C
{
public:
  C();
  virtual double foo (double i);
};

int A::foo (int i)
{
  return i + 1;
}

int B::foo (int i)
{
  return i + 2;
}

double C::foo (double i)
{
  return i + 3.5;
}

static int __attribute__ ((noinline)) middleman (class A *obj, int i)
{
  return obj->foo (i);
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

__attribute__ ((always_inline)) C::C ()
{
}

A::A ()
{
}

static  __attribute__ ((noinline)) void bah ()
{
  class B b;

  C *c = new ((void *) &b.data) C;

  if (middleman (&b, get_input ()) != 3)
    abort ();
}

int main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < 10; i++)
    bah ();
  return 0;
}
