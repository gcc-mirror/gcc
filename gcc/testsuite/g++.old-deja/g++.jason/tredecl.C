// Bug:  Foo<Bar> *p semi-instantiates Foo<Bar> in local scope, so
// when Foo<Bar> f tries to instantiate it later, it only finds the partial
// instantiation from before.
//
// No PR; distilled from James Clark's SGML project.
//
// Build don't link:

class Bar { };

template<class T> class Foo;

Foo<Bar> *p;

template<class T> class Foo { };

Foo<Bar> f;			// gets bogus error - hosed binding levels
