/*
TEST_OUTPUT:
---
fail_compilation/fail351.d(14): Error: cast(uint)this.num[index] is not an lvalue
---
*/

// 2780

struct Immutable {
    immutable uint[2] num;

    ref uint opIndex(size_t index) immutable {
        return num[index];
    }
}

void main() {
    immutable Immutable foo;
    //foo[0]++;
}
