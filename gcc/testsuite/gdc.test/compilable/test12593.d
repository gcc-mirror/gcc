int[R] aa;  // Place before the declaration of key struct

struct R
{
    int opCmp(ref const R) const { return 0; }

    //bool opEquals(ref const R) const { return true; }
    //size_t toHash() const nothrow @safe { return 0; }
}

void main()
{}
