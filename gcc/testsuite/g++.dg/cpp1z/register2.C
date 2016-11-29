// P0001R1 - C++17 removal of register keyword
// { dg-do compile }
// { dg-options "-Wno-register" }

#if defined(__i386__) || defined(__x86_64__)
#define REG1 "ebx"
#define REG2 "edi"
#endif

#ifdef REG1
register int a __asm (REG1);	// { dg-bogus "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } }
#endif
register int b;			// { dg-bogus "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } }
register int c ();		// { dg-error "storage class 'register' invalid for function" }
int foo (register int d)	// { dg-bogus "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } }
{
  return d;
}
int bar ()
{
#ifdef REG2
  register int e __asm (REG2);	// { dg-bogus "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } }
#else
  int e;
#endif
  register int f;		// { dg-bogus "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } }
  e = 6;
  f = 7;
  return e + f;
}
