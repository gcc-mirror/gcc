// { dg-do assemble  }

struct S {
  static const char* cp = "abc"; // { dg-error "" } initialization of non-const
};
