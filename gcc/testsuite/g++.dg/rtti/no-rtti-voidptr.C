// { dg-do compile }
// { dg-options "-fno-rtti" }

// PR C++/28687

struct A {
  virtual ~A() { }
};

struct B : A {
};

A* f()
{
  return new B();
}

int main()
{
  void* b = dynamic_cast<void*>(f());
}
