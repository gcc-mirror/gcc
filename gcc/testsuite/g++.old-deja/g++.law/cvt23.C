// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <9308051209.AA15962@NeoSoft.Com>
// From: daniels@sugar.neosoft.com (Brad Daniels)
// Subject: Bug in g++ 2.4.5: Conversion operators to enumerated types don't wor
// Date: Thu, 5 Aug 93 12:09:09 CDT

enum bar { AAA, BBB };

class foo {
   bar a, b;
public:
   foo(bar i, bar j) : a(i),b(j) {}
   operator const bar() const { return a; }
   bar f() { return *this; }
   void g(foo &c) { b = c; }
};
