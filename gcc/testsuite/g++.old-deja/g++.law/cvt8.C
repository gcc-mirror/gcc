// Build don't link: 
// GROUPS passed conversions
// cvt file
// From: solomon@cs.wisc.edu (Marvin Solomon)
// Message-Id: <9209141509.AA23124@gjetost.cs.wisc.edu>
// Subject: g++ 2.2.2 seems to be forgetting a "const"
// Date: Mon, 14 Sep 92 10:09:58 -0500

extern "C" void printf(...);

struct A {
        int i;
};

struct B {
        int i;
        operator const A&() const;
};

B::operator const A&() const {
        static A a;
        a.i = i;
        printf("convert B to A at %x\n", &a);
        return a;
}

void f(A &a) { // ERROR - in passing argument
        printf("A at %x is %d\n", &a, a.i);
}

int main() {
        B b;
        b.i = 99;
        f(b);// ERROR - .*
}
