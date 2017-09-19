/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-Wno-pmf-conversions" } */

struct Klass
{
  int implementation ();
  int magic ();

  typedef int (Klass::*MemFuncPtr)();

  static MemFuncPtr resolver ();
};

Klass::MemFuncPtr p = &Klass::implementation;

int Klass::implementation (void)
{
  __builtin_printf ("'ere I am JH\n");
  return 1234;
}


Klass::MemFuncPtr Klass::resolver (void)
{
  return &Klass::implementation;
}

int f (void) __attribute__ ((ifunc ("foo")));

typedef int (F)(void);
extern "C" F* foo () { return 0; }


int Klass::magic (void) __attribute__ ((ifunc ("_ZN5Klass8resolverEv")));

int main ()
{
  Klass obj;

  return !(obj.magic () == 1234);
}
