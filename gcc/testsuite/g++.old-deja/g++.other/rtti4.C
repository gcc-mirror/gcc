// { dg-do run  }
// { dg-options "-frtti -w" }
// test of rtti of single inheritance and multiple inheritance with 
// virtual inheritance

#include <typeinfo>

extern "C" {
  int printf(const char *, ...);
  void exit(int);
}

class X {
 public:
  int xi;
  virtual int f() {};
};

class Y : public virtual X {
  short ys;
};

class Z : public virtual Y {
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
  virtual int a() {};
};

class B {
public:
  int Bi;
  virtual int g() {};
};

class D : public virtual A, private B {
  int Di;
};

class E : public virtual D, public B {
  int Ei;
};

class F : public E, public virtual D {
  int Fi;
};

D d;
A *ap = &d;
B *bp = (B *)&d;
F f;
A *aap = &f;
D *dp = &f;
B *bbp = (B *)dp;

void *vp = zp;

void error  (int i)
{
  exit(i);
}

int main ()
{
  if (typeid(z) != typeid(Z)) error(1);
  if (typeid(*yp) != typeid(Z)) error(2);
  if (typeid(*yp) != typeid(*zp)) error(3);
  if (typeid(xp) == typeid(yp)) error(4);

  xp = (X *)&y;
  if (typeid(*xp) == typeid(*yp)) error(5);
  if (typeid(*xp) != typeid(Y)) error(6);
  
  if (typeid(*ap) != typeid(*bp)) error (31);
  if (typeid(*ap) != typeid(D)) error(32);
  if (typeid(*aap) != typeid(*bbp)) error(33);
  if (typeid(*dp) != typeid(*aap)) error(34);
}
