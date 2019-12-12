struct Array(T)
{
    private struct Payload
    {
        size_t _capacity;
        T[] _payload;

        ~this()
        {
        }

        this(this)
        {
        }
    }
    private alias RefCounted!(Payload) Data;
    private Data _data;

    bool opEquals(ref const Array rhs) const
    {
        return true;
    }
}


struct BinaryHeap(Store)
{
    private static struct Data
    {
        Store _store;
        size_t _length;
    }
    private RefCounted!(Data) _payload;
}


struct RefCounted(T)
{
    struct RefCountedStore
    {
        private struct Impl
        {
            T _payload;
            size_t _count;
        }

        private Impl* _store;

    }
    RefCountedStore _refCounted;

    this(this)
    {
    }

    ~this()
    {
        .destroy(_refCounted._store._payload);
    }

    void opAssign(typeof(this) rhs)
    {
    }

    void opAssign(T rhs)
    {
        typeid(T).destroy(&rhs);
    }
}
