// https://issues.dlang.org/show_bug.cgi?id=24157

/*
TEST_OUTPUT:
---
fail_compilation/test24157.d(23): Error: cannot take address of expression `p.self()` because it is not an lvalue
fail_compilation/test24157.d(27): Error: cannot take address of expression `p.unshared()` because it is not an lvalue
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
