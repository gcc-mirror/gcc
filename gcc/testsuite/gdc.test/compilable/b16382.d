// REQUIRED_ARGS: -c
/*
TEST_OUTPUT:
---
&this
---
*/
struct S0 {
    void foo() {
        pragma(msg, &this);
    }
}
