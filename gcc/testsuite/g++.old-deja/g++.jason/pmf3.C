// { dg-do assemble  }
// From: enewton@uunet.uu.NET
// Subject: g++ 2.5.8: cannot cast member function pointers
// Date: 27 Jan 1994 01:22:56 -0500

struct A {
	void f(char);
	void g(int);
};

typedef void (A::*Ptr)(char);

void q() {
   Ptr p;

   p  = (Ptr) &A::f;
   p  = (Ptr) &A::g;
   p  = &A::f;
}
