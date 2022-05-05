/*
TEST_OUTPUT:
---
fail_compilation/fail131.d(8): Error: function `D main` parameter list must be empty or accept one parameter of type `string[]`
---
*/

int main(lazy char[][] args)
{
    return args.length;
}
