struct X {};
X& X::*PTM_1;		// ERROR - pointer to reference member
void X::*PTM_2;		// ERROR - pointer to void member

struct A {
  static int& ir;
};

int i;
int& A::ir = i;			// not an error
