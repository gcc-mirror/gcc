// PR c++/17155
// { dg-do link }

struct A {
  virtual ~A() {}
};


void tsk_tsk(void)
{
  struct B : public A {
    B(int) {}
  };
}

int main () {}
