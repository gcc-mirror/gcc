// Test for default-initialization of POD-structs in functional cast notation.

struct foo { int a[10]; };

int main()
{
  foo f = foo();
  int r = 0;
  for (int i = 0; i < 10; ++i)
    r |= f.a[i];
  return r;
}
