// test of rtti of non-class types
// Special g++ Options: -frtti

#include <typeinfo>

extern "C" {
  int printf(const char *, ...);
  void exit(int);
}

int i;
short s;
char c;
long l;

unsigned int ui;
unsigned short us;
unsigned char uc;
unsigned long ul;

float f;
double d;

int& ri = i;
const volatile int cvi = 10;
volatile const int vci = 20;
const int ci = 100;

int *pi;
int ai[10];

enum color { red, blue, green, yellow};

int (*fp)();
int (*gp)();
int (*hp)(int);

class XX {
public:
  int xxi;
  float xxf;
  int xxf1 () { return 0; };
  int xxf2 (int k) { return 0; };
};

class YY {
public:
  int yyi;
  double yyd;
  int yyf1 (float f) { return 0; };
  double yyf2 () {return yyd;};
};

int XX::*ptmd1;
int XX::*ptmd2;
float XX::*ptmd3;
int YY::*ptmd4;

int (XX::*ptmf1) ();
int (XX::*ptmf2) ();
int (XX::*ptmf3) (int);
int (YY::*ptmf4) ();

int func1 ()
{ return 0;}

int func2 ()
{ return 1;}

int func3 (int i)
{ return i;}

short func4 ()
{ return 99;}

void error  (int i)
{
  exit(i);
}

int main ()
{
  if (typeid(i) != typeid(int)) error(1);
  if (typeid(s) != typeid(short)) error(2);
  if (typeid(c) != typeid(char)) error(3);
  if (typeid(l) != typeid(long)) error(4);
  if (typeid(ui) != typeid(unsigned int)) error(5);
  if (typeid(us) != typeid(unsigned short)) error(6);
  if (typeid(uc) != typeid(unsigned char)) error(7);
  if (typeid(ul) != typeid(unsigned long)) error(8);
  if (typeid(f) != typeid(float)) error(9);
  if (typeid(d) != typeid(double)) error(10);

  if (typeid(*pi) != typeid(int)) error(51);
  if (typeid(pi) == typeid(ai)) error(52);
  if (typeid(ri) != typeid(i)) error(53);
  if (typeid(cvi) != typeid(vci)) error (54);
  if (typeid(vci) != typeid(i)) error(55);
  if (typeid(ci) != typeid(cvi)) error (56);
  if (typeid(ci) != typeid(const int)) error(57);

  if (typeid(func1) != typeid(func2)) error (81);
  if (typeid(func2) == typeid(func3)) error (82);
  if (typeid(func1) == typeid(func4)) error (83);
  if (typeid(func3) == typeid(func4)) error (84);

  if (typeid(red) != typeid(color)) error (101);
  if (typeid(green) != typeid(blue)) error (102);

  if (typeid(fp) != typeid(gp)) error (103);
  if (typeid(gp) == typeid(hp)) error (104);

  if (typeid(ptmd1) != typeid(ptmd2)) error (105);
  if (typeid(ptmd1) == typeid(ptmd3)) error (106);
  if (typeid(ptmd2) == typeid(ptmd4)) error (107);

  if (typeid(ptmf1) != typeid(ptmf2)) error (108);
  if (typeid(ptmf2) == typeid(ptmf3)) error (109);
  if (typeid(ptmf1) == typeid(ptmf4)) error (110);
  return 0;
}
