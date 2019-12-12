struct RefCounted(T)
{
    struct RefCountedStore
    {
        private struct Impl
        {
            T _payload;
        }

        private Impl* _store;
    }
    RefCountedStore _refCounted;

    ~this()
    {
        import core.stdc.stdlib : free;
    }
}

struct GroupBy(R)
{
    struct SharedInput
    {
        Group unused;
    }

    struct Group
    {
        private RefCounted!SharedInput _allGroups;
    }
}

void main()
{
    GroupBy!(int[]) g1;
}
