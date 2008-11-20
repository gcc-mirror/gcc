// PR c++/28513

class foo {			// { dg-error "changes meaning" }
public:
  typedef int bar;
};

class baz {
public:
  foo::bar foo;			// { dg-error "declaration" }
};
