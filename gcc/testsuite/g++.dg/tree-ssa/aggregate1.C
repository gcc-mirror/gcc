// Test that we don't bother building a cleanup for the last aggregate element.
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not {A::~A \(&b\.a} "gimple" } }

struct A
{
  A(int);
  ~A();
};

struct B
{
  A a;
};

int main()
{
  B b = { 1 };
}
