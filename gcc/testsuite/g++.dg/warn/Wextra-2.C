// { dg-options "-Wextra" } 

struct S {
  S();
};

struct T {
private:
  int i;
public:
  // There should be no warning about this data member because the
  // default constructor for "T" will invoke the default constructor
  // for "S", even though "S" is "const".
  const S s; // { dg-bogus "const" }
};
