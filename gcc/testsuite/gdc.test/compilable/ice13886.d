// REQUIRED__ARGS:
// PERMUTE_ARGS:

struct Y()
{
    this() {}
    ~this() { this = null; }
    ref opAssign(S)(S) { }
}

void main()
{
    static if (is(typeof({ Y!(); }))) {}
}
