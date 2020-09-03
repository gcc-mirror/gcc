// https://bugzilla.gdcproject.org/show_bug.cgi?id=186
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S186
{
    union
    {
        struct
        {
            ubyte fieldA;
            byte  fieldB = -1;
            byte fieldC = -1;
        }
        size_t _complete;
    }

    this(size_t complete)
    {
        this._complete = complete;
    }
}

static if (size_t.sizeof == 8)
    enum checkval = 0x0200000000000002;
else
    enum checkval = 0x02000002;

void check186(in S186 obj, byte fieldB)
{
    assert(obj.fieldA == 2);
    assert(obj.fieldB == 0);
    assert(obj.fieldC == 0);
    assert(obj._complete == checkval);
    assert(fieldB == 0);
}

void test186(size_t val)
{
    S186 obj = S186(val);
    check186(obj, obj.fieldB);

    assert(obj.fieldA == 2);
    assert(obj.fieldB == 0);
    assert(obj.fieldC == 0);
    assert(obj._complete == checkval);

    obj = S186(val);
    check186(obj, obj.fieldB);

    assert(obj.fieldA == 2);
    assert(obj.fieldB == 0);
    assert(obj.fieldC == 0);
    assert(obj._complete == checkval);
}

void main()
{
    test186(checkval);
}
