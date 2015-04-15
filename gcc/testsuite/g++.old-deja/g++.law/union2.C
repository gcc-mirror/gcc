// { dg-do assemble  }
// GROUPS passed unions
// anon-union file
// From: gerlek@dat.cse.ogi.edu (Michael Gerlek)
// Date:     Tue, 8 Dec 92 12:56 PST
// Subject:  private anonymous unions have public members? (gcc-2.3.1)
// Message-ID: <m0mzByL-0000hoC@dat.cse.ogi.edu>

class A {
public:
  int x;
private:
  int y;    // { dg-message "" } private
  union {
    int z;  // { dg-message "" } private
  };
};

void f() {
  A a;

  a.x = 0;
  a.y = 1;// { dg-error "" } .*
  a.z = 2;// { dg-error "" } 
}
