// https://github.com/dlang/dmd/issues/21478

// Test struct that implements "rule of five"

struct S21478
{
    // 1. destructor
    ~this() { }
    // 2. copy constructor
    this(ref return scope S21478) { assert(0); }
    // 3. copy assign
    void opAssign(const ref S21478) { }
    // 4. move constructor
    this(return scope S21478) { assert(0); }
    // 5. move assign
    void opAssign(const S21478) { assert(0); }
}

void main()
{
    S21478 sa, sb;
    sb = sa;    // Should call 3, not 2 + 5.
}
