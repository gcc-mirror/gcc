// REQUIRED_ARGS: -unittest

void main()
{
    MinHeap!(int) foo = new MinHeap!(int)();
}

class MinHeap(NodeType)
{
    unittest
    {
        struct TestType {}
        MinHeap!(TestType) foo = new MinHeap!(TestType)();
    }
}
