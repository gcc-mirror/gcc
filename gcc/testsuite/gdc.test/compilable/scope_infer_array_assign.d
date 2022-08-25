// REQUIRED_ARGS: -preview=dip1000

// Test that scope inference works even with non POD array assignment
// This is tricky because it gets lowered to something like:
// (S[] __assigntmp0 = e[]) , _d_arrayassign_l(this.e[], __assigntmp0) , this.e[];

@safe:

struct File
{
    void* f;
    ~this() scope { }
}

struct Vector
{
    File[] e;

    auto assign(File[] e)
    {
        this.e[] = e[]; // slice copy
    }
}

void test(scope File[] arr, Vector v)
{
    v.assign(arr);
}
