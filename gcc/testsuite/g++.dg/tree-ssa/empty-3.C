// PR c++/20408
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "struct Foo" 2 "gimple" } }

struct Foo {};
void foo(const Foo&);
void bar(Foo);

void fooc(void)
{
        foo(Foo());
}
void barc(void)
{
        bar(Foo());
}
