// PR c++/28513

class foo {			// { dg-message "declared here" }
public:
  typedef int bar;
};

class baz {
public:
  foo::bar foo;			// { dg-error "changes meaning" }
};
