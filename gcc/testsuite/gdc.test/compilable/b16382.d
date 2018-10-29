// REQUIRED_ARGS: -c
struct S0 {
    void foo() {
        pragma(msg, &this);
    }
}
