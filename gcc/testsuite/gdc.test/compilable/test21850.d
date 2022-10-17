// https://issues.dlang.org/show_bug.cgi?id=21850

struct Strukt2 {
    this(int* _block) {  }
}

struct Strukt {
    int* block;
    Strukt2 foo() { return Strukt2(null); }
    alias foo this;
}

bool wrapper(T)(ref T a, ref T b)
{
    return doesPointTo(a, b);
}

void johan() pure {
    Strukt a;
    Strukt b;
    assert(wrapper(a, b));         // error wrapper is not pure
    assert(doesPointTo(a, b));     // fine
}

bool doesPointTo(S, T)(S , T) {
    return false;
}

bool doesPointTo(S)(shared S) {
    return false;
}

bool mayPointTo(){
    return false;
}
