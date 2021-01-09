/*
TEST_OUTPUT:
---
fail_compilation/fail5733.d(12): Error: `opDispatch!"foo"` isn't a template
---
*/
struct Test
{
    struct opDispatch(string dummy)
    { enum opDispatch = 1; }
}
auto temp = Test().foo!(int);
