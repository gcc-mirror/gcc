// https://bugzilla.gdcproject.org/show_bug.cgi?id=183
// { dg-do compile }

struct S183a
{
    union I183a
    {
        struct
        {
            double x, y, z;
        }
        struct
        {
            double a, b, c;
        }
    }

    I183a inner;

    this(double x, double y, double z)
    {
        this.inner.x = x;
        this.inner.y = y;
        this.inner.z = z;
    }
}

struct S183b
{
    @property get()
    {
        union Buf
        {
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}

struct S183c
{
    @property get()
    {
        union Buf
        {
            TypeInfo info;
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}

void test183()
{
    auto v1 = S183a(0, 0, 0);
    auto v2 = S183b().get;
    auto v3 = S183c().get;
}
