alias T = MyStruct!float;

struct MyStruct(U)
{
    U x;
    U y;

    this(U xx, U yy)
    {
        x = xx;
        y = yy;
    }

    MyStruct!U opBinary(string op)(MyStruct!U z) const
    {
        alias C = typeof(return);
        auto w = C(this.x, this.y);
        return w.opOpAssign!(op)(z);
    }

    MyStruct!U opBinaryRight(string op)(MyStruct!U z) const
    {
        return opBinary!(op)(z);
    }

    ref MyStruct opOpAssign(string op, U)(const MyStruct!U z)
    {
        mixin ("x "~op~"= z.x;");
        mixin ("y "~op~"= z.y;");
        return this;
    }

    MyStruct!U opBinary(string op, R)(R z) const
        if (is(R == int) || is(R == float))
    {
        alias C = typeof(return);
        auto w = C(this.x, this.y);
        return w.opOpAssign!(op)(z);
    }

    MyStruct!U opBinaryRight(string op, R)(R z) const
        if (is(R == int) || is(R == float))
    {
        return opBinary!(op)(z);
    }

    ref MyStruct opOpAssign(string op, R)(const R z)
        if (is(R == int) || is(R == float))
    {
        mixin ("x "~op~"= z;");
        return this;
    }
}

void main()
{
    T c = MyStruct!float(1.0f, 1.0f);
    T[] arr = [T(1,1), T(2,2), T(3,3), T(4,4), T(5,5), T(6,6)];
    T[] result = new T[arr.length];

    // part 2

    result[0] = c * c;
    assert(result[0] == T(1, 1));

    result[0] = arr[1] * arr[2];
    assert(result[0] == T(6, 6));

    int[] intarr = [6, 5, 4, 3, 2, 1];

    result[] = arr[] * arr[];
    assert(result[] == [T(1, 1), T(4, 4), T(9, 9), T(16, 16), T(25, 25), T(36, 36)]);

    result[] = arr[] * 3;
    assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);
    result[] = 3 * arr[];
    assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);

    result[] = arr[];
    result[1..3] = arr[1..3] * 2.0f;
    assert(result[] == [T(1, 1), T(4, 2), T(6, 3), T(4, 4), T(5, 5), T(6, 6)]);
    result[1..3] = 2.0f * arr[1..3];
    assert(result[] == [T(1, 1), T(4, 2), T(6, 3), T(4, 4), T(5, 5), T(6, 6)]);

    result[] = arr[];
    result[1..$] = arr[1..$] * 2.0f;
    assert(result[] == [T(1, 1), T(4, 2), T(6, 3), T(8, 4), T(10, 5), T(12, 6)]);
    result[1..$] = 2.0f * arr[1..$];
    assert(result[] == [T(1, 1), T(4, 2), T(6, 3), T(8, 4), T(10, 5), T(12, 6)]);

    result[] = intarr[] * arr[];
    assert(result[] == [T(6, 1), T(10, 2), T(12, 3), T(12, 4), T(10, 5), T(6, 6)]);
    result[] = arr[] * intarr[];
    assert(result[] == [T(6, 1), T(10, 2), T(12, 3), T(12, 4), T(10, 5), T(6, 6)]);

    result[] = intarr[] * T(2,3);
    assert(result[] == [T(12, 3), T(10, 3), T(8, 3), T(6, 3), T(4, 3), T(2, 3)]);
    result[] = T(2,3) * intarr[];
    assert(result[] == [T(12, 3), T(10, 3), T(8, 3), T(6, 3), T(4, 3), T(2, 3)]);

    result[] = intarr[] * c;
    assert(result[] == [T(6, 1), T(5, 1), T(4, 1), T(3, 1), T(2, 1), T(1, 1)]);
    result[] = c * intarr[];
    assert(result[] == [T(6, 1), T(5, 1), T(4, 1), T(3, 1), T(2, 1), T(1, 1)]);

    result[] = arr[];
    result[1..3] = intarr[1..3] * c;
    assert(result[] == [T(1, 1), T(5, 1), T(4, 1), T(4, 4), T(5, 5), T(6, 6)]);
    result[1..3] = c * intarr[1..3];
    assert(result[] == [T(1, 1), T(5, 1), T(4, 1), T(4, 4), T(5, 5), T(6, 6)]);

    result[1..$] = intarr[1..$] * c;
    assert(result[] == [T(1, 1), T(5, 1), T(4, 1), T(3, 1), T(2, 1), T(1, 1)]);
    result[1..$] = c * intarr[1..$];
    assert(result[] == [T(1, 1), T(5, 1), T(4, 1), T(3, 1), T(2, 1), T(1, 1)]);

    result[] = arr[];
    result[1..3] = intarr[1..3] * arr[1..3];
    assert(result[] == [T(1, 1), T(10, 2), T(12, 3), T(4, 4), T(5, 5), T(6, 6)]);
    result[1..3] = arr[1..3] * intarr[1..3];
    assert(result[] == [T(1, 1), T(10, 2), T(12, 3), T(4, 4), T(5, 5), T(6, 6)]);

    result[] = [1,2,3,4,5,6] * c;
    assert(result[] == [T(1, 1), T(2, 1), T(3, 1), T(4, 1), T(5, 1), T(6, 1)]);
    result[] = c * [1,2,3,4,5,6];
    assert(result[] == [T(1, 1), T(2, 1), T(3, 1), T(4, 1), T(5, 1), T(6, 1)]);

    result[] = arr[] * [1,2,3,4,5,6];
    assert(result[] == [T(1, 1), T(4, 2), T(9, 3), T(16, 4), T(25, 5), T(36, 6)]);
    result[] = [1,2,3,4,5,6] *  arr[];
    assert(result[] == [T(1, 1), T(4, 2), T(9, 3), T(16, 4), T(25, 5), T(36, 6)]);

    result[] = [c, 2 * c, 3 * c, 4 * c, 5 * c, 6 * c] * [c, 2 * c, 3 * c, 4 * c, 5 * c, 6 * c];
    assert(result[] == [T(1, 1), T(4, 1), T(9, 1), T(16, 1), T(25, 1), T(36, 1)]);

    result[] = [c, 2 * c, 3 * c, 4 * c, 5 * c, 6 * c] * [1,2,3,4,5,6];
    assert(result[] == [T(1, 1), T(4, 1), T(9, 1), T(16, 1), T(25, 1), T(36, 1)]);
    result[] = [1,2,3,4,5,6] * [c, 2 * c, 3 * c, 4 * c, 5 * c, 6 * c];
    assert(result[] == [T(1, 1), T(4, 1), T(9, 1), T(16, 1), T(25, 1), T(36, 1)]);

    result[] = arr[] * c;
    assert(result[] == [T(1, 1), T(2, 2), T(3, 3), T(4, 4), T(5, 5), T(6, 6)]);
    result[] = c * arr[];
    assert(result[] == [T(1, 1), T(2, 2), T(3, 3), T(4, 4), T(5, 5), T(6, 6)]);

    result[] = c * 3.0f * arr[];
    assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);
    result[] = 3.0f * c * arr[];
    assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);

    result[] = arr[] * 3.0f * c;
    assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);
    // result[] = arr[] * c * 3.0f; //not ok
    // assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);

    result[] = 3.0f * arr[] * c;
    assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);
    // result[] = c * arr[] * 3.0f; //not ok
    // assert(result[] == [T(3, 1), T(6, 2), T(9, 3), T(12, 4), T(15, 5), T(18, 6)]);

    result[] = c * arr[] * c;
    assert(result[] == [T(1, 1), T(2, 2), T(3, 3), T(4, 4), T(5, 5), T(6, 6)]);
}
