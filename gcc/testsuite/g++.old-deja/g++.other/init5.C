// Objects must be destructed in decreasing cnt order
// Original test attributed to James Kanze <jkanze@otelo.ibmmail.com>
// execution test - XFAIL *-*-*

static int cnt;

class A {
  int myCnt;
public:
  A() : myCnt(cnt++) {}
  ~A() { if (--cnt != myCnt) abort(); }
};

void f() { static A a; /* a.myCnt == 1 */ }

class B {
  int myCnt;
public:
  B() : myCnt(cnt+1) { f(); ++cnt; }
  ~B() { if (--cnt != myCnt) abort(); }
};

static A a1; // a1.myCnt == 0
static B b1; // b1.myCnt == 2
static A a2; // a2.myCnt == 3

int main() {}
