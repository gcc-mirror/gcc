//PR c++ 29024

typedef static int a;   // { dg-error "conflicting" }
typedef register int b; // { dg-error "conflicting" }
typedef extern int c;   // { dg-error "conflicting" }
static typedef int a;   // { dg-error "conflicting" }

int foo()
{
  typedef auto int bar; // { dg-error "conflicting" }
}
