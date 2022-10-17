void main() {
    @(1) enum { A }
    /// comment
    @(1) enum X { A }
    @(2) enum Y;
    @(1) @(2) enum Z { A }

    struct Test { int test; }
    @Test(1) enum W { A }
    @(1) enum V: int { A }
    X a;
    static assert(__traits(getAttributes, X).length == 1);
    static assert(__traits(getAttributes, X)[0] == 1);
}
