// DR 569, Spurious semicolons at namespace scope should be allowed
// PR c++/113760
// { dg-options "" }

// C++11 allows extra semicolons at namespace scope.
struct S {
  void foo();
};
;

void S::foo () {
};
;

namespace N {
};
;

void f();
;

void
f ()
{
};
;

int x;
;
