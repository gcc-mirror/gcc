// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100882
// { dg-do compile }

struct AllocatorList(Factory)
{
    Factory factory;
    auto make(size_t n) { return factory(n); }
    this(Factory plant)
    {
        factory = plant;
    }
}

struct Region
{
    ~this()
    {
    }
}

auto mmapRegionList()
{
    struct Factory
    {
        this(size_t )
        {
        }
        auto opCall(size_t )
        {
            return Region();
        }
    }
    auto shop = Factory();
    AllocatorList!Factory(shop);
}
