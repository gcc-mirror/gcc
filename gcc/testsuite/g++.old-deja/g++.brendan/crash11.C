// { dg-do assemble  }
// GROUPS passed old-abort
extern "C" int printf (const char *, ...);

class A {
        int	i; // { dg-message "" } private
        int	j; // { dg-message "" } private
    public:
	int	h;
	A() { i=10; j=20; }
	virtual void f1() { printf("i=%d j=%d\n",i,j); }
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); } // { dg-error "9:virtual functions cannot be friends" }
};

class B : public A {
    public:
	virtual void f1() { printf("i=%d j=%d\n",i,j); }// { dg-error "" }  member.*// ERROR -  member.*
	friend virtual void f2() { printf("i=%d j=%d\n",i,j); }  // { dg-error "9:virtual functions cannot be friends" }
// { dg-error "private" "" { target *-*-* } .-1 }
};

int
main() {
	A * a = new A;
}
