// PR c++/85600
// { dg-do run }

struct A
{
  virtual ~A() { }
};

struct B: A { };

A *p;
int count;

A *f() {
  ++count;
  return p;
}

int main()
{
  p = new B;
  delete f();
  if (count != 1)
    __builtin_abort();
}
