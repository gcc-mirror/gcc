// GROUPS passed ARM-compliance
// arm file
// From: Johan Bengtsson <jbn@lulea.trab.se>
// Date:     Thu, 21 Oct 93 16:10:25 +0100
// Subject:  gcc 2.4.5 initializes base classes in mem-initializer order
// Message-ID: <9310211510.AA14943@holden.lulea.trab.se>

#include <stdio.h>

int state = 0;

class A { public:
        A() { 
		if (state == 0)
			state = 1;
		else {
			printf ("FAIL\n");
			exit (1);
		}
	}
};

class B { public:
        B() {
		if (state == 1)
			state = 2;
		else {
			printf ("FAIL\n");
			exit (1);
		}
	}
};

class AB : public A, public B { public:
        AB() : B(), A() { 
		if (state == 2)
			state = 3;
		else {
			printf ("FAIL\n");
			exit (1);
		}
	}
};

int main()
{
        AB ab;
	if (state == 3)
		printf("PASS\n");
	else
		printf("FAIL\n");
	exit (state != 3);
}
