// { dg-do assemble  }

class X{
    unsigned int i;
  public:
    void f();
};

void X::f()
{
  union {
    int foo[sizeof (i)];
  };
}
