// { dg-do run  }
// { dg-options "-frtti" }
// test of rtti of single inheritance and multiple inheritance classes

#include <typeinfo>

extern "C" {
  int printf(const char *, ...);
  void exit(int);
}

class X {
 public:
  int xi;
};

class Y : public X {
  short ys;
};

class Z : public Y {
  int zi;
};

Z z;
Y y;
Y *yp = &z;
X *xp = &z;
Z *zp = &z;

class A {
 public:
  int Ai;
};

class B {
 public:
  int Bi;
};

class D : public A, public B {
  int Di;
};

/*
class E : public D, public B {
  int Ei;
};
*/
class E {
  int Ei;
};

class F : public E, public D {
  int Fi;
};

D d;
A *ap = &d;
B *bp = &d;
F f;
A *aap = &f;
D *dp = &f;
B *bbp = dp;

void *vp = zp;

void error  (int i)
{
  exit(i);
}

int main ()
{
  if (typeid(z) != typeid(Z)) error(1);
  if (typeid(*yp) == typeid(Z)) error(2);
  if (typeid(*yp) == typeid(*zp)) error(3);
  if (typeid(xp) == typeid(yp)) error(4);

  xp = (X *)&y;
  if (typeid(*xp) == typeid(*yp)) error(5);
  if (typeid(*xp) == typeid(Y)) error(6);
  
  if (typeid(*ap) == typeid(*bp)) error (31);
  if (typeid(*ap) == typeid(D)) error(32);

  if (typeid(*aap) == typeid(*bbp)) error(33);
  if (typeid(*dp) == typeid(*aap)) error(34);
}
