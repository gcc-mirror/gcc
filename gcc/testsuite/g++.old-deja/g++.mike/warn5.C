// { dg-do assemble  }
// { dg-options "-Wpointer-arith" }

double X(const double x) { return x; }
double Y() { return 1.0; }
double Z() { return 2.0; }

struct A {
  void bar() { }
  void foo() { }
};

typedef void (A::*pmf)();

static int mememe = &A::foo - &A::bar;	// { dg-error "invalid operands" } 
pmf b = &A::foo-1;	// { dg-error "invalid operands" } 

int main() {
    double y;
    y=X(Y-Z);   // { dg-error "pointer to a function" } 
}
