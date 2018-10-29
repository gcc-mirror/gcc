/*
TEST_OUTPUT:
---
fail_compilation/fail133.d(13): Error: function D main circular dependency. Functions cannot be interpreted while being compiled
fail_compilation/fail133.d(15):        called from here: main()
---
*/

template t(int t)
{
}

int main()
{
    return t!(main() + 8);
}
