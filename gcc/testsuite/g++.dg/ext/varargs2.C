// { dg-do run }

int c;
struct X { X() {}; X(const X&) { ++c; } };
void Foo (X, ...) {}
void bin (X &p)
{
  Foo (p, p);
}

int main()
{
  X x;
  bin(x);
  if (c != 2)
    __builtin_abort();
}
