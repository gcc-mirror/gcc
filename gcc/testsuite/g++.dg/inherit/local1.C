// PR c++/17121

struct A {
  virtual ~A() {}
};

void tsk_tsk()
{
  struct B : public A {};
}
