// Build don't link: 
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
  int y;    // ERROR - private
  union {
    int z;  // ERROR - private
  };
};

void f() {
  A a;

  a.x = 0;
  a.y = 1;// ERROR - .*
  a.z = 2;// ERROR - 
}
