// Build don't link: 
// GROUPS passed access
// access file
// Message-Id: <9211281852.AA24557@cove.cis.ufl.edu>
// From: Robert Forsman <thoth@cove.cis.ufl.edu>
// Subject: method access bug in gcc-2.3.1 on a sparc-sun-sunos4.1.2
// Date: Sat, 28 Nov 92 13:52:14 EST

extern "C" {
    int atoi(const char*);
}

struct thingus;

class foo {
public:
  static const foo alpha;
  static const foo beta;

private:
  int i;
  foo(thingus * s);
public:
  foo() {i=0;}
};

struct thingus {
  int i;
};

static thingus blah, blah2;

const foo foo::alpha(&blah);
const foo foo::beta(&blah2);
