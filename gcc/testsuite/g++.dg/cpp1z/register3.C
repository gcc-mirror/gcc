// P0001R1 - C++17 removal of register keyword
// { dg-do compile { target c++14_down } }
// { dg-options "-Wregister" }

#if defined(__i386__) || defined(__x86_64__)
#define REG1 "ebx"
#define REG2 "edi"
#endif

#ifdef REG1
register int a __asm (REG1);	// { dg-bogus "'register' storage class specifier used" }
#endif
register int b;			// { dg-warning "'register' storage class specifier used" }
register int c ();		// { dg-error "1:storage class 'register' invalid for function" }
int foo (register int d)	// { dg-warning "'register' storage class specifier used" }
{
  return d;
}
int bar ()
{
#ifdef REG2
  register int e __asm (REG2);	// { dg-bogus "'register' storage class specifier used" }
#else
  int e;
#endif
  register int f;		// { dg-warning "'register' storage class specifier used" }
  e = 6;
  f = 7;
  return e + f;
}
