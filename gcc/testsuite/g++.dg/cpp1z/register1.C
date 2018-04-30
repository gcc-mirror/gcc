// P0001R1 - C++17 removal of register keyword
// { dg-do compile }

#if defined(__i386__) || defined(__x86_64__)
#define REG1 "ebx"
#define REG2 "edi"
#endif

#ifdef REG1
register int a __asm (REG1);	// { dg-bogus "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
#endif
register int b;			// { dg-error "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
register int c ();		// { dg-error "storage class 'register' invalid for function" }
int foo (register int d)	// { dg-error "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
{
  return d;
}
int bar ()
{
#ifdef REG2
  register int e __asm (REG2);	// { dg-bogus "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
#else
  int e;
#endif
  register int f;		// { dg-error "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
  e = 6;
  f = 7;
  return e + f;
}
