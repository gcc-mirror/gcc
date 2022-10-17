// https://issues.dlang.org/show_bug.cgi?id=20422

/*
TEST_OUTPUT:
---
fail_compilation/issue20422.d(11): Error: missing length argument for array
---
*/

void main() {
    new int[];
}
