// { dg-do assemble  }
// Origin: Brendan Kehoe <brendan@cygnus.com>

 template <int x> int foo(char[4][x]) { return x; }
 int (*bar)(char[4][3]) = &foo;
