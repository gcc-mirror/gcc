/*
TEST_OUTPUT:
---
fail_compilation/test17908b.d(13): Error: function `test17908b.foobar` cannot be used because it is annotated with `@disable`
---
*/
void foobar() {}
@disable void foobar(int) {}
alias i = foobar;

void main()
{
    i(10);
}
