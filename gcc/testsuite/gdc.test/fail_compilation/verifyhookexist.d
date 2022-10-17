/*
REQUIRED_ARGS: -checkaction=context
EXTRA_SOURCES: extra-files/minimal/object.d
*/

/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/verifyhookexist.d(22): Error: `object.__ArrayCast` not found. The current runtime does not support casting array of structs, or the runtime is corrupt.
fail_compilation/verifyhookexist.d(28): Error: `object.__equals` not found. The current runtime does not support equal checks on arrays, or the runtime is corrupt.
fail_compilation/verifyhookexist.d(29): Error: `object.__cmp` not found. The current runtime does not support comparing arrays, or the runtime is corrupt.
fail_compilation/verifyhookexist.d(33): Error: `object._d_assert_fail` not found. The current runtime does not support generating assert messages, or the runtime is corrupt.
fail_compilation/verifyhookexist.d(36): Error: `object.__switch` not found. The current runtime does not support switch cases on strings, or the runtime is corrupt.
fail_compilation/verifyhookexist.d(41): Error: `object.__switch_error` not found. The current runtime does not support generating assert messages, or the runtime is corrupt.
---
*/

struct MyStruct { int a, b; }
MyStruct[] castToMyStruct(int[] arr) {
    return cast(MyStruct[])arr;
}

void test() {
    int[] arrA, arrB;

    bool a = arrA[] == arrB[];
    bool b = arrA < arrB;

    {
        int x = 1; int y = 1;
        assert(x == y);
    }

    switch ("") {
    default:
        break;
    }

    final switch (0) {
    case 1:
        break;
    }
}
