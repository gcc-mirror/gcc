// Test for using prefix attributes in an abstract declarator.
// Contributed by Jason Merrill <jason@cygnus.com>
// Skip if not target: i?86-*-*
// Build don't link:

void f (void (__attribute__((__stdcall__)) *)());
