// Bug: expand_vec_init doesn't copy arrays of builtin types.

struct B {
  B() { }
  B(const B&) { }
};

struct A
{
  B b;
  int ar[5];
};

int main()
{
  A a;
  for (int i = 0; i < 5; ++i)
    a.ar[i] = i;

  A a2 = a;

  for (int i = 0; i < 5; ++i)
    if (a2.ar[i] != a.ar[i])
      return 1;
}
