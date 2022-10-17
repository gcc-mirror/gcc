// https://issues.dlang.org/show_bug.cgi?id=20236

/*
TEST_OUTPUT:
---
---
*/

struct X
{
    alias y this;
    deprecated int y() { return 5; }
    int x() { return 5; }
}

void main()
{
    static void func(int) {}
    with(X.init) {
        func(x);
    }
}
