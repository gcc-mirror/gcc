/*
TEST_OUTPUT:
---
fail_compilation/bug8891.d(21): Error: need `this` for `opCall` of type `S(int n)`
---
*/

struct S
{
    int value = 10;
    S opCall(int n) // non-static
    {
        //printf("this.value = %d\n", this.value);    // prints garbage!
        S s;
        s.value = n;
        return s;
    }
}
void main()
{
    S s = 10;
}
