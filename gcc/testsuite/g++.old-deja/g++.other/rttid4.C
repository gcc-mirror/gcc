// test of rtti of single inheritance and multiple inheritance with 
// virtual inheritance
// dynamic casting
// Special g++ Options: -w

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
F *fp = &f;
A *aap = &f;
D *dp = &f;
E *ep = &f;
B *bbp = (B *)dp;

void *vp = zp;

/*
void error (int i)
{
  printf("FAIL\n");
  exit(i);
}
*/

void error  (int i)
{
  exit(i);
}

int main ()
{
  vp = (void *)0;

  vp = dynamic_cast<Y *> (&z);
  if (vp == 0) error(11);

  vp = dynamic_cast<Z *> (yp);
  if (vp == 0) error(11);

  vp = dynamic_cast<X *> (yp);
  if (vp == 0) error(12);

  vp = dynamic_cast<D *> (dp);
  if (vp != (void *)dp) error(21);

  // Ill-formed: dynamic_cast to private or ambiguous base
  //   vp = dynamic_cast<B *> (dp);
  //   if (vp == (void *)dp) error(21);

  //   vp = dynamic_cast<B *> (fp);
  //   if (vp == (void *)bbp) error(22);

  vp = dynamic_cast<void *> (aap);
  if (vp != (void *)fp) error(23);

  vp = dynamic_cast<B *> (aap);
  if (vp == (void *)bbp) error(24);

}
