// { dg-do assemble  }
struct X {};
X& X::*PTM_1;		// { dg-error "" } pointer to reference member
void X::*PTM_2;		// { dg-error "" } pointer to void member

struct A {
  static int& ir;
};

int i;
int& A::ir = i;			// not an error
