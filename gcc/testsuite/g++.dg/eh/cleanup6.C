// Test that we don't wrap the non-throwing A cleanup with a B cleanup.

// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "B::~B" 1 "gimple" } }

struct A { A(); ~A(); };
struct B { B(const A& = A()); ~B(); };

int main()
{
  B b;
}
