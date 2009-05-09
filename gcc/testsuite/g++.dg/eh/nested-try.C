// { dg-do compile }
// Nested try statements shadowing each other was crashing in EH edge redirection.
// PR middle-end/40043
struct A
{
  ~A();
  void foo();
};

void bar()
{
  A a;

  try
  {
    A b;

    try
    {
      b.foo();
    }
    catch (int) {}
  }
  catch (int) {}
}
