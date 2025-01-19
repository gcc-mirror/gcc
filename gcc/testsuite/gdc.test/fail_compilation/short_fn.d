/*
TEST_OUTPUT:
---
fail_compilation/short_fn.d(13): Error: can only return void expression, `this` call or `super` call from constructor
fail_compilation/short_fn.d(14): Error: can only return void expression, `this` call or `super` call from constructor
---
*/

struct Number
{
    int x;

    this(int x) => this.x = x;
    this(byte x) => Number();
}
