// PR c++/65143
// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not "vptr" gimple } }

struct A
{
    int j;
};

struct B : public virtual A
{
};

struct C final : public B
{
    int get();
};

int C::get()
{
    return A::j;
}
