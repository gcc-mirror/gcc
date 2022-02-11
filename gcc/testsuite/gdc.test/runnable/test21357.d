// https://issues.dlang.org/show_bug.cgi?id=21357
// PERMUTE_ARGS:
struct BatchState
{
    int[10] arr;

    BatchState copy()
    {
        auto ret = BatchState(arr);
        arr[0] += 1;
        return ret;
    }
}

struct GrayArea
{
    BatchState low;

    this(this)
    {
        low = low.copy;
    }
}

void main()
{
    GrayArea a;
    a.low.arr[0] = 1;
    GrayArea b;
    b.low.arr[0] = 4;
    b = a; // calls the postblit

    assert(a.low.arr[0] == 1);
    assert(b.low.arr[0] == 1);
}
