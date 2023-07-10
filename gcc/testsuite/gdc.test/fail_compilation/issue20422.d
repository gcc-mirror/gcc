// https://issues.dlang.org/show_bug.cgi?id=20422

/*
REQUIRED_ARGS: -m32
TEST_OUTPUT:
---
fail_compilation/issue20422.d(15): Error: missing length argument for array
fail_compilation/issue20422.d(16): Error: negative array dimension `-1`
fail_compilation/issue20422.d(17): Error: negative array dimension `-2147483648`
fail_compilation/issue20422.d(18): Error: too many arguments for array
---
*/

void main() {
    new int[];
    new int[-1];
    new int[](int.min);
    new int[](1, 2);
}
