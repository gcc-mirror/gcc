struct Bar {
  int i[8];
};
struct Bar foo(struct Bar **p)
{
  return foo((struct Bar**)*p);
}

