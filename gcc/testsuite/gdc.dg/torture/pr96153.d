// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96153
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
struct Checked(T, Hook)
{
    private T payload;
    Hook hook;

    size_t toHash() const nothrow @safe
    {
        return hashOf(payload) ^ hashOf(hook);
    }
}

Checked!(T, Hook) checked(Hook, T)(const T value)
{
    return Checked!(T, Hook)(value);
}

@system unittest
{
    static struct Hook1
    {
        uint var1 = uint.max;
        uint var2 = uint.max;
    }

    assert(checked!Hook1(12).toHash() != checked!Hook1(13).toHash());
    assert(checked!Hook1(13).toHash() == checked!Hook1(13).toHash());

    static struct Hook2
    {
        uint var1 = uint.max;
        ushort var2 = ushort.max;
    }

    assert(checked!Hook2(12).toHash() != checked!Hook2(13).toHash());
    assert(checked!Hook2(13).toHash() == checked!Hook2(13).toHash());

    static struct Hook3
    {
        ulong var1 = ulong.max;
        uint var2 = uint.max;
    }

    assert(checked!Hook3(12).toHash() != checked!Hook3(13).toHash());
    assert(checked!Hook3(13).toHash() == checked!Hook3(13).toHash());
}
