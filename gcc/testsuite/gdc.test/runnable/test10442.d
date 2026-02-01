// EXTRA_FILES: imports/test10442a.d
module test10442;
import imports.test10442a;

struct T
{
    int x;
    void* p;
}

void main()
{
    // assumes enum RTInfo(T) merges identical bitmaps
    assert(typeid(T).rtInfo !is null); // ok
    assert(typeid(S).rtInfo is typeid(T).rtInfo); // fails
}
