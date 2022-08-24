/*
TEST_OUTPUT:
---
fail_compilation/bug5096.d(13): Error: unmatched closing brace
---
*/
void foo(int x)
    in {
        assert(x > 0);
    } do {
        x++;
    }
}
void main() {}
