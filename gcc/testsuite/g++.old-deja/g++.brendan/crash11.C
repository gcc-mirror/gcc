// Build don't link: 
// GROUPS passed old-abort
extern "C" int printf (const char *, ...);

class A {
        int	i; // ERROR - private
        int	j; // ERROR - private
    public:
	int	h;
	A() { i=10; j=20; }
	virtual void f1() { printf("i=%d j=%d\n",i,j); }
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }// ERROR -  virtual.*
};

class B : public A {
    public:
	virtual void f1() { printf("i=%d j=%d\n",i,j); }// ERROR -  member.*// ERROR -  member.*
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }// ERROR -  virtual.*// ERROR -  member.*// ERROR -  member.*
};

int
main() {
	A * a = new A;
}
