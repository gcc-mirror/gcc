// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <9304121647.AA25819@tnt>
// From: mclaugh@tnt.acsys.com (Mark A. McLaughlin)
// Subject: g++ bug
// Date: Mon, 12 Apr 93 10:47:01 MDT

#include <iostream>

class B {
};

class A : public B {
   short s;
public:
   A(short _s = 0) { s = _s; }
   operator const short &() const { return s; }
};

   int
main() {
   A a(37);
   //cout << a << endl;
   std::cout << (short)a << std::endl;

   return 0;
}  // main


