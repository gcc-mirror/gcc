// PRMS Id: 4679
// Bug: redeclaration of templates erases the definition.
// Build don't link:

template <class T> class Foo { public: void h(); };
template <class T> class Foo;

void g()
{
  Foo<int> f;
}
