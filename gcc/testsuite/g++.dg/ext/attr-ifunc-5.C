// PR c/81854 - weak alias of an incompatible symbol accepted
// { dg-do compile }
// { dg-require-ifunc "" } */

struct Klass
{
  int implementation ();
  const char* magic ();

  typedef int (Klass::*MemFuncPtr)();

  static MemFuncPtr resolver ();
};

int Klass::implementation (void)
{
  return 0;
}

const char* __attribute__ ((ifunc ("_ZN5Klass8resolverEv")))
  Klass::magic ();   // { dg-warning "alias between functions of incompatible types" }



Klass::MemFuncPtr
Klass::resolver (void) // { dg-message "aliased declaration here" }
{
  return &Klass::implementation;
}
