// { dg-do assemble  }

struct S
{
  void f()
    {
      const int i; // { dg-error "" } uninitialized const
    }
};
