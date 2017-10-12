// PR c/81854 - weak alias of an incompatible symbol accepted
// { dg-do compile }
// { dg-require-ifunc "" } */
// { dg-options "-Wextra -Wno-pmf-conversions" }

struct Klass
{
  int implementation ();
  int good_magic ();
  int iffy_magic ();
  const char* bad_magic ();

  typedef int (Func)(Klass*);
  typedef int (Klass::*MemFuncPtr)();

  static Func* good_resolver ();
  static void* iffy_resolver ();
  static MemFuncPtr bad_resolver ();
};

int Klass::implementation (void)
{
  return 0;
}

// Verify no warning for the expected/compatible declaration.

int __attribute__ ((ifunc ("_ZN5Klass13good_resolverEv")))
Klass::good_magic ();

Klass::Func*
Klass::good_resolver (void)
{
  MemFuncPtr mfp = &Klass::implementation;

  return reinterpret_cast<Func*>(mfp);
}


// Verify a warning for the unsafe declaration.

int __attribute__ ((ifunc ("_ZN5Klass13iffy_resolverEv")))
Klass::iffy_magic ();    // { dg-message "resolver indirect function declared here" }

void*
Klass::iffy_resolver (void)   // { dg-warning ".ifunc. resolver for .int Klass::iffy_magic\\(\\). should return .int \\(\\*\\)\\(Klass\\*\\)." }
{
  MemFuncPtr mfp = &Klass::implementation;

  return reinterpret_cast<void*>(mfp);
}


// Verify an error for an incompatible declaration.

const char* __attribute__ ((ifunc ("_ZN5Klass12bad_resolverEv")))
Klass::bad_magic ();   // { dg-message "resolver indirect function declared here" }


Klass::MemFuncPtr
Klass::bad_resolver (void)   // { dg-error ".ifunc. resolver for .const char\\* Klass::bad_magic\\(\\). must return .const char\\* \\(\\*\\)\\(Klass\\*\\)." }
{
  return &Klass::implementation;
}
