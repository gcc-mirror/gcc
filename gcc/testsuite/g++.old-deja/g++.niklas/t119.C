// { dg-do assemble  }
// GROUPS passed niklas i386
unsigned long foo(unsigned long x)
{
  return x & ~0104000;	// { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated" "" { target c++29 } }
}
