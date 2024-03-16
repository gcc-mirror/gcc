// https://issues.dlang.org/show_bug.cgi?id=24157

/*
TEST_OUTPUT:
---
fail_compilation/test24157.d(23): Error: `p.self()` is not an lvalue and cannot be modified
fail_compilation/test24157.d(27): Error: `p.unshared()` is not an lvalue and cannot be modified
---
*/

class Promise {
    auto ref self() {
        return this;
    }

    auto ref unshared() shared {
        return cast() this;
    }
}


void testThis(Promise p) {
    auto ptr = &p.self(); // must not return a ref to the Promise class ref
}

void testCastThis(shared Promise p) {
    auto ptr = &p.unshared(); // ditto
}
