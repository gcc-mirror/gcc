// Special g++ Options: -w
// GROUPS passed ARM-compliance
// arm file (also in cvt file)
// Message-Id: <9303061246.AA09402@gjetost.cs.wisc.edu>
// From: solomon@cs.wisc.edu (Marvin Solomon)
// Subject: Incorrect resolution of conversion path
// Date: Sat, 6 Mar 93 06:46:27 -0600


extern "C" int printf (const char *, ...);

class Base {
public:
	int i;
	Base(int ii) : i(ii) {}
};

class Derived : public Base {
public:
	Derived(int ii) : Base(ii) {}
	operator Base&();
};

Derived::operator Base&() {
	Base *b = new Base(100*i);
	return *b;
}

int f(Base &b) {
	if (b.i == 99)
	  { printf ("PASS\n"); return 0; }
	else
	  { printf ("FAIL\n"); return 1; }
}

int main() {
	Derived d(99);
	return f(d);
}

