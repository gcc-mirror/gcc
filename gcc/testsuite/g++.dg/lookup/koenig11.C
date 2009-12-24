// Test that we treat unions like other classes in arg-dep lookup.

union U
{
  friend void f (U);
};

int main()
{
  U u;
  f(u);
}
