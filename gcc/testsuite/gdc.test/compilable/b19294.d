alias MT = MyStruct!int;

struct MyStruct(T)
{
    T x;

    this(T y)
    {
        x = y;
    }

    MyStruct!T opBinary(string op)(MyStruct!T y) const
    {
        alias C = typeof(return);
        auto w = C(this.x);
        return w.opOpAssign!(op)(y);
    }

    MyStruct!T opBinaryRight(string op)(MyStruct!T y) const
    {
        return opBinary!(op)(y);
    }

    ref MyStruct opOpAssign(string op, T)(const MyStruct!T z)
    {
        mixin ("x "~op~"= z.x;");
        return this;
    }

    MyStruct!T opBinary(string op)(T y) const
    {
        alias C = typeof(return);
        auto w = C(this.x);
        return w.opOpAssign!(op)(y);
    }

    MyStruct!T opBinaryRight(string op)(T y) const
    {
        return opBinary!(op)(y);
    }

    ref MyStruct opOpAssign(string op, T)(const T z)
    {
        mixin ("x "~op~"= z;");
        return this;
    }
}

void test()
{
    MT s = MyStruct!int(1);
    MT[] arr = [s, 2 * s, 3 * s, 4 * s, 5 * s, 6 * s];
    MT[] result = new MT[arr.length];

    result[] = arr[] + s;
    result[] = s + arr[];

    result[] = arr[] - s;
    result[] = s - arr[];

    result[] = arr[] * s;
    result[] = s * arr[];

    result[] = arr[] / s;
    result[] = s / arr[];

    result[] = arr[] ^^ s;
    result[] = s ^^ arr[];
}
