// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <9308051530.AA05922@NeoSoft.Com>
// From: daniels@sugar.neosoft.com (Brad Daniels)
// Subject: g++ 2.4.5:  Conversion operators to complex types don't work either
// Date: Thu, 5 Aug 93 15:30:04 CDT

class bar { int x; public: bar() : x(1){} };

class foo {
    bar a, b;
public:
    foo(bar i, bar j) : a(i),b(j) {}
    operator const bar() const { return a; }
    const bar f() { return *this; }
    void g(foo &c) { b = c; }
};
