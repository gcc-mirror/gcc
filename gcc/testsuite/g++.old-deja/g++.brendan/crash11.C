// { dg-do assemble  }
// GROUPS passed old-abort
extern "C" int printf (const char *, ...);

class A {
        int	i; // { dg-error "" } private
        int	j; // { dg-error "" } private
    public:
	int	h;
	A() { i=10; j=20; }
	virtual void f1() { printf("i=%d j=%d\n",i,j); }
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }// { dg-error "" }  virtual.*
};

class B : public A {
    public:
	virtual void f1() { printf("i=%d j=%d\n",i,j); }// { dg-error "" }  member.*// ERROR -  member.*
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }// { dg-error "" }  virtual.*// ERROR -  member.*// ERROR -  member.*
};

int
main() {
	A * a = new A;
}
