// Build don't link:
class A{
  public:
    enum Foo{f1,f2}; // gets bogus error - XFAIL

    class B{
      friend class A;
      Foo f;
      public:
        B():f(f1){}  // gets bogus error (inaccessible) - XFAIL
    };
};
