// PRMS Id: 4484 (bug 2)
// Bug: g++ does not grok abstract declarator syntax for method pointers.
// Build don't link:

template <class T> class A { };
void (A<int>::*p)() = (void (A<int>::*)())0; // gets bogus error - abstract declarator failure
