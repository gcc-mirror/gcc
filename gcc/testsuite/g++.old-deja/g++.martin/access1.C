// { dg-do assemble  }
class A{
  public:
    enum Foo{f1,f2};

    class B{
      friend class A;
      Foo f;
      public:
        B():f(f1){} 
    };
};
