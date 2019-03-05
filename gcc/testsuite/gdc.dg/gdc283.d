// https://bugzilla.gdcproject.org/show_bug.cgi?id=283
// { dg-do run { target hw } }

struct Impl
{
    size_t _count;
}

struct RefCountedStore
{
    Impl* _store;

    void initialize()
    {
        import core.stdc.stdlib : malloc;
        _store = cast(Impl*) malloc(Impl.sizeof);
        _store._count = 1;
    }

    bool isInitialized()
    {
        return _store !is null;
    }

    void ensureInitialized()
    {
        if (!isInitialized)
            initialize();
    }
}

struct RefCounted14443
{
    RefCountedStore _refCounted;

    this(int)
    {
        _refCounted.initialize();
    }

    this(this)
    {
        ++_refCounted._store._count;
    }

    ~this()
    {
        if (--_refCounted._store._count)
            return;

        import core.stdc.stdlib : free;
        free(_refCounted._store);
        _refCounted._store = null;
    }

    int refCountedPayload()
    {
        _refCounted.ensureInitialized();
        return 1;
    }
}

struct PathRange14443
{
    RefCounted14443 path;

    @property PathElement14443 front()
    {
        return PathElement14443(this, path.refCountedPayload());
    }
}

struct PathElement14443
{
    PathRange14443 range;

    this(PathRange14443 range, int)
    {
        this.range = range;
    }
}

void main()
{
    auto path = RefCounted14443(12);
    if (path._refCounted._store._count != 1)
        assert(0);
    {
        auto _r = PathRange14443(path);
        if (path._refCounted._store._count != 2)
            assert(0);
        {
            auto element = _r.front;
            if (path._refCounted._store._count != 3)
                assert(0);
        }
        if (path._refCounted._store._count != 2)
            assert(0);
    }
    if (path._refCounted._store._count != 1)
        assert(0);
}
