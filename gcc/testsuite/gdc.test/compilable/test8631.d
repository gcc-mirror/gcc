// PERMUTE_ARGS:
// REQUIRED_ARGS: -de

class B {
    int foo() immutable { return 2; }
    int foo() const { return 2; }
}
class D : B {
    override int foo() immutable { return 2; }
             int foo() const shared { return 2; }
    override int foo() const { return 2; }
}
