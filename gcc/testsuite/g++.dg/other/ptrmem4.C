// Bug: This checks that the pointer-to-member-function type is not
// shared between differently-qualified pointer-to-method types.

// { dg-do compile }
struct A
{ 
  void f () {}
};

void (A::*const cp)() = &A::f;

int main ()
{ 
  void (A::* p)();
  void (A::** ip)() = &p;

  *ip = &A::f;
}
