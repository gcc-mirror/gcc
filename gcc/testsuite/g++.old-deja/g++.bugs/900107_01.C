// { dg-do run  }
// g++ 1.36.1 bug 900107_01

// Unlike GCC and Cfront 2.0, the g++ 1.36.1 compiler gives struct, union,
// and class declarations which are nested within blocks file scope.

// Cfront 2.0 passes this test.

// keywords: block scope, file scope, nested types, tagged types

class c1 { int c1_member1; };
struct s1 { int s1_member1; };
union u1 { int u1_member1; };
enum e1 { e1_val1 };
typedef int t1;

void foo ()
{
  class c1 {			// { dg-bogus "" } 
    int c1_member1;
  } c1_local_object_0;

  struct s1 {			// { dg-bogus "" } 
    int s1_member1;
  } s1_local_object_0;

  union u1 {			// { dg-bogus "" } 
    int u1_member1;
  } u1_local_object_0;

  enum e1 {		// OK using g++ or GCC, but mishandled by Cfront 2.0.
    e1_value1
  } e1_local_object_0;

  typedef int t1;		// OK
}

int main () { return 0; }
