// PR c++/14452
// We got confused trying to preevaluate the new-initializer.

struct S {}; 
void foo (bool b) 
{ 
  new S(b ? S() : S()); 
} 
