module traits_getVirtualIndex;

class VirtualIndexBase
{
    protected int _foo;

    int doubler()
    {
        return foo * 2;
    }

    @property int foo() const
    {
        return _foo;
    }

    @property void foo(int val)
    {
        _foo = val;
    }

    final void finalFunc()
    {

    }
}

class VirtualIndexDerived : VirtualIndexBase
{
    @property override int foo() const
    {
        return super.foo;
    }

    @property override void foo(int val)
    {
        super.foo = val;
    }

    @property @safe int foo() pure nothrow
    {
        return _foo * 2;
    }

    final void otherFinalFunc()
    {

    }
}

final class VirtualIndexFinal : VirtualIndexDerived
{
    @property final override int foo() const
    {
        return super.foo;
    }

    @property final override void foo(int val)
    {
        super.foo = val;
    }

    @property @safe final override int foo() pure nothrow
    {
        return super.foo;
    }
}

private @property ptrdiff_t getIndex(T, string m, size_t index = 0)()
{
    return __traits(getVirtualIndex, __traits(getOverloads, T, m)[index]);
}

void main()
{
    ptrdiff_t doublerIndex = getIndex!(VirtualIndexBase, "doubler");
    assert(doublerIndex > 0);
    ptrdiff_t firstIndex = getIndex!(VirtualIndexBase, "foo", 0);
    ptrdiff_t secondIndex = getIndex!(VirtualIndexBase, "foo", 1);
    assert(firstIndex > 0 && secondIndex > 0);
    // Virtual index is in definition order.
    assert(secondIndex == firstIndex + 1);
    assert(firstIndex == doublerIndex + 1);
    ptrdiff_t finalIndex = getIndex!(VirtualIndexBase, "finalFunc");
    assert(finalIndex == -1);
    assert(getIndex!(VirtualIndexDerived, "doubler") == doublerIndex);
    assert(getIndex!(VirtualIndexDerived, "foo", 0) == firstIndex);
    assert(getIndex!(VirtualIndexDerived, "foo", 1) == secondIndex);
    assert(getIndex!(VirtualIndexDerived, "finalFunc") == finalIndex);
    assert(getIndex!(VirtualIndexDerived, "otherFinalFunc") == -1);
    ptrdiff_t newOverloadIndex = getIndex!(VirtualIndexDerived, "foo", 2);
    assert(newOverloadIndex == secondIndex + 1);
    foreach(i, overload; __traits(getOverloads, VirtualIndexFinal, "foo"))
    {
        // It should still return the initial virtual index even if overridden to be final.
        ptrdiff_t finalOverrideIndex = getIndex!(VirtualIndexFinal, __traits(identifier, overload), i);
        ptrdiff_t originalIndex = getIndex!(VirtualIndexDerived, __traits(identifier, overload), i);
        assert(finalOverrideIndex == originalIndex);
    }
}
