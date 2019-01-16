/*
TEST_OUTPUT:
---
fail_compilation/ice10259.d(11): Error: circular reference to 'ice10259.D.d'
fail_compilation/ice10259.d(11):        called from here: (*function () => x)()
---
*/
class D
{
    int x;
    D d = { auto x = new D(); return x; }();
}
enum x = new D;

/*
TEST_OUTPUT:
---
fail_compilation/ice10259.d(25): Error: circular reference to 'ice10259.D2.d'
fail_compilation/ice10259.d(25):        called from here: (*function () => x)()
---
*/
class D2
{
    int x;
    D2 d = function { auto x = new D2(); return x; }();
}
enum x2 = new D2;
