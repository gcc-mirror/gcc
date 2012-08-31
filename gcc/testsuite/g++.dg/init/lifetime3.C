// PR c++/26714
// { dg-do run }

extern "C" void abort();

bool ok = false;
struct A {
  A() { }
  ~A() { if (!ok) abort(); }
};

struct B {
  static A foo() { return A(); }
};

B b_g;

struct scoped_ptr {
  B* operator->() const { return &b_g; }
  B* get() const { return &b_g; }
};

B *get() { return &b_g; }

int main()
{
  scoped_ptr f;
  const A& ref1 = f->foo();
  const A& ref2 = f.get()->foo();
  const A& ref3 = get()->foo();
  const A& ref4 = B::foo();
  B *pf = f.get();
  const A& ref5 = pf->foo();


  ok = true;
}
