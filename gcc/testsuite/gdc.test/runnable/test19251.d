string result;

struct A
{
    int[] a;
    immutable(A) fun()
    {
        result ~= "Yo";
        return immutable A([7]);
    }

    alias fun this;
}

void main()
{
    A a;
    immutable A b = a;   // error: cannot implicitly convert expression a of type A to immutable(A)
    assert(result == "Yo");
}
