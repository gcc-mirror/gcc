/* TEST_OUTPUT:
---
fail_compilation/ice23564.d(10): Error: cannot construct nested class `FreeList` because no implicit `this` reference to outer class `RBTree` is available
---
*/
class BlockHeader
{
    this()
    {
        new FreeList;
    }
}

class RBTree
{
    class FreeList
    {
    }

    void _each_reverse()
    {
    }
}

alias FreeList = RBTree.FreeList;
