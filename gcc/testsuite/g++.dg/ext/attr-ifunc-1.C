/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-Wno-pmf-conversions" } */

struct Klass
{
  int a[4];

  int implementation ();
  int magic ();

  /* An ifunc resolver must return a pointer to an ordinary (non-member)
     function.  To make it possible to use ifunc with member functions,
     the resolver must convert a member function pointer to an ordinary
     function pointer (slicing off the high word).  */
  typedef int Func (Klass*);

  static Func* resolver ();
};

int Klass::implementation ()
{
  __builtin_printf ("'ere I am JH\n");
  return a[0] + a[1] + a[2] + a[3];
}

Klass::Func* Klass::resolver (void)
{
  /* GCC guarantees this conversion to be safe and the resulting pointer
     usable to call the member function using ordinary (i.e., non-member)
     function call syntax.  */

  return reinterpret_cast<Func*>(&Klass::implementation);
}

int f (void) __attribute__ ((ifunc ("foo")));

typedef int (F)(void);
extern "C" F* foo () { return 0; }


int Klass::magic () __attribute__ ((ifunc ("_ZN5Klass8resolverEv")));

int main ()
{
  Klass obj;

  obj.a[0] = 1;
  obj.a[1] = 2;
  obj.a[2] = 3;
  obj.a[3] = 4;

  return !(obj.magic () == 10);
}
