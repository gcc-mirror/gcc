/* RUN_OUTPUT:
---
inside switch: 1
---
*/

int get() { return 1; }

void test() {
    import core.stdc.stdio : printf;
    switch (auto x = get()) {
        default:
            printf("inside switch: %d\n", x);
    }
}

void main() {
    test();
}
