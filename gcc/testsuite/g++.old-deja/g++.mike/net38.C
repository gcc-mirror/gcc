// { dg-do run  }
struct A {
  virtual int a ()       { return 0; }
};

struct B {
  virtual int b ()       { return 0; }
  virtual int b2 ()      { return 0; }
};

struct C : public A, public B {
  virtual int a ()       { return 1; }
  virtual int b ()       { return 2; }
  virtual int b2 ()      { return 3; }
};

int (C::*vmpb) () = &C::b;
int (C::*vmpb2) () = &C::b2;
int (C::*vmpa) () = &C::a;

int main () {
  C c;
  if ((c.*vmpa)() != 1)
    return 1;
  if ((c.*vmpb)() != 2)
    return 1;
  if ((c.*vmpb2)() != 3)
    return 1;
}
