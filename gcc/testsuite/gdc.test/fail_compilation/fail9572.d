/*
TEST_OUTPUT:
---
fail_compilation/fail9572.d(10): Error: index type `ubyte` cannot cover index range 0..300
---
*/

void main() {
    int[300] data;
    foreach (ubyte i, x; data) {}
}
