// https://bugzilla.gdcproject.org/show_bug.cgi?id=286
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void main()
{
    struct K286
    {
        int count;
        this(this)
        {
            count++;
        }
    }

    struct S286
    {
        int data;
        this(K286 key)
        {
            data = key.count;
        }
    }

    S286 getData(K286 key)
    {
        static S286[K286] getCache;
        auto p = key in getCache;
        if (p)
            return *p;
        return (getCache[key] = S286(key));
    }

    auto s = getData(K286());
    if (s.data == 0)
        assert(0);
}
