// { dg-do assemble { target i?86-*-* } }
// { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } }
// Test for using prefix attributes in a parameter decl.
// Contributed by Jason Merrill <jason@cygnus.com>

#define _stdcall __attribute__ ((__stdcall__))

typedef void (_stdcall* pfn)();

void f (void (_stdcall*)    ());
void f (void (_stdcall* pfn)());
