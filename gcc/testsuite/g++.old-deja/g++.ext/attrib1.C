// Test for using prefix attributes in a parameter decl.
// Contributed by Jason Merrill <jason@cygnus.com>
// Skip if not target: i?86-*-*
// Build don't link:

#define _stdcall __attribute__ ((__stdcall__))

typedef void (_stdcall* pfn)();

void f (void (_stdcall*)    ());
void f (void (_stdcall* pfn)());
