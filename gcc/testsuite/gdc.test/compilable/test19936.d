// REQUIRED_ARGS: -de

struct Bla
{
    deprecated("bla")
    int get() { return 5; }

    alias get this;
}

void main()
{
    Bla[] blaArray;
    // ~= should not try to call `.get`, because there's no indication that
    // `blaArray` has any kind of opAppendAssign related overload in the first place.
    blaArray ~= Bla();
}
