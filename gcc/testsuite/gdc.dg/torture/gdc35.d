// https://bugzilla.gdcproject.org/show_bug.cgi?id=35
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

/**
 * Here the BinaryHeap instance uses an alias parameter and therefore
 * the instance's functions (percolateDown) need to be generated in
 * topNIndex->BinaryHeap scope and not in the declaration scope
 * (module->BinaryHeap).
 */
void topNIndex()()
{
    bool indirectLess(int a, int b)
    {
        return a > b;
    }

    auto a = BinaryHeap!(indirectLess)();
}

struct BinaryHeap(alias less)
{
    void percolateDown()
    {
        less(0, 1);
    }
}

void test35a()
{
    topNIndex();
}

/*
 * Similar as test35a but with an additional indirection.
 * The nested function chain for percolateDown should look like this:
 * topNIndex2->BinaryHeap2->percolateDown.
 */
void topNIndex2()()
{
    bool indirectLess(int a, int b)
    {
        return a > b;
    }
    auto a = BinaryHeap2!(S35b!(indirectLess)())();
}

struct S35b(alias a)
{
    void foo()
    {
        a(0, 0);
    }
}

struct BinaryHeap2(alias less)
{
    void percolateDown()
    {
        less.foo();
    }
}

void test35b()
{
    topNIndex2();
}

void main()
{
    test35a();
    test35b();
}
