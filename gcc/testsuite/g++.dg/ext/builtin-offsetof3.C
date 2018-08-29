// PR c++/85061
// { dg-do compile }

struct B { int a, b; };
struct A
{
  static int x[2];
  static int y;
  static B z;
};

int i = __builtin_offsetof (volatile A, x[0]);	// { dg-error "cannot apply 'offsetof' to static data member 'A::x'" }
int j = __builtin_offsetof (volatile A, y);	// { dg-error "cannot apply 'offsetof' to static data member 'A::y'" }
int k = __builtin_offsetof (volatile A, z.a);	// { dg-error "cannot apply 'offsetof' to a non constant address" }
