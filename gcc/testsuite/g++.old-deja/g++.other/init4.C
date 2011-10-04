// { dg-options "-std=c++98 -pedantic-errors" }
// { dg-do assemble  }

class error {
public:
  error(int) {}
};

class foo {
  const error x = 1; // { dg-error "" } initialization of non-static data member
};


