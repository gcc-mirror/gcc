struct A {
  struct B { } *B;
  int i, j, k, l, m;
};

struct A *t;

int
main ()
{
  void *p = t
    ->B;
}
