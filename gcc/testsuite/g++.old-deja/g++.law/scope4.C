// { dg-do assemble  }
// GROUPS passed scoping
// local-class file
// From: daniels@sugar.neosoft.com (Brad Daniels)
// Date:     Thu, 5 Aug 93 15:36:36 CDT
// Subject:  Bug in g++ 2.4.5: Can't touch nested class identifier inside its members
// Message-ID: <9308051536.AA06115@NeoSoft.Com>

void f() {
    class foo {
        int x;
    public:
        foo() : x(1) {}
        int bar() { foo p; return p.x; }
    };
}
