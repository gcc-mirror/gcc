// Build don't link:

struct S {
  static const char* cp = "abc"; // ERROR - initialization of non-const
};
