struct C {
  static const int x = 24;
};

struct A
{
  struct B: C
  {
    enum { E = x };
  };

  // OK, earlier x was found in a base, lookup didn't pass through A.
  static const int x = 42;
};
