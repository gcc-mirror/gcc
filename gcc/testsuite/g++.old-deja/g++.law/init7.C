// { dg-do assemble  }
// GROUPS passed initialization
class test
 {
  public:
   int x;
   int y;
   test (int val) { x = val; y = 0;}
 };

test ar(34)[5];// { dg-error "" } .*
