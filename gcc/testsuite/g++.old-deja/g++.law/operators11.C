// Build don't link: 
// GROUPS passed operators
// opr-eq file
// Message-Id: <CCJrut.9M7@csc.ti.com>
// From: rowlands@hc.ti.com (Jon Rowlands)
// Subject: g++ 2.4.5: assignment operator in base class
// Date: Mon, 30 Aug 1993 00:54:29 GMT

class B {
public:
	B &	operator = (B);	// delete this line and problem goes away
};

class D : public B {
public:
	D();
	D(int);
	D(B);
};

int
main() {
	B	b;
	D	d;

	d = d;

	d = 0;	// t.cxx:20: assignment not defined for type `D'
	d = D(0);

	d = b;	// t.cxx:23: assignment not defined for type `D'
	d = D(b);

	return(0);
}

