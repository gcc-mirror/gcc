const int x = 24;
struct A
{
  struct B
  {
    enum { E = x };
  };
  static const int x = 42;	// { dg-error "changes meaning" }
};
