/*
TEST_OUTPUT:
---
fail_compilation/fail11503b.d(17): Error: cannot implicitly convert expression `makes()` of type `immutable(int[])` to `int[]`
---
*/
immutable int[] x = [1, 2, 3];

auto makes() pure
{
    return x;
}

int main()
{
    auto a = x;
    int[] b = makes();
    return b[1];
}
