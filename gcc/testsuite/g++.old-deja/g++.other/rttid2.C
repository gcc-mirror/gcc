// test of rtti of single inheritance and multiple inheritance classes
// dynamic casting
// Special g++ Options: -frtti

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
F *fp = &f;
A *aap = &f;
D *dp = &f;
B *bbp = dp;

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

  vp = dynamic_cast<X *> (yp);
  if (vp == 0) error(12);

  vp = dynamic_cast<D *> (dp);
  if (vp != (void *)dp) error(21);

  vp = dynamic_cast<B *> (fp);
  if (vp != (void *)bbp) error(22);

}

