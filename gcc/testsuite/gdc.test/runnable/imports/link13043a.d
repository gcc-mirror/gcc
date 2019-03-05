module imports.lin13043a;

struct QualifiedNameTests
{
    struct Inner
    {
        const int opCmp(ref const Inner) { return 0; }
    }

    shared(const(Inner[string])[]) data;

  version(bug)
    size_t toHash() const
    {
        return typeid(typeof(data)).getHash(cast(const void*)&data);
    }
}
