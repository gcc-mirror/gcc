// Build don't link:

struct S
{
  void f()
    {
      const int i; // ERROR - uninitialized const
    }
};
