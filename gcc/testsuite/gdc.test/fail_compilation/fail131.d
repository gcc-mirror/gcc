/*
TEST_OUTPUT:
---
fail_compilation/fail131.d(8): Error: function D main parameters must be main() or main(string[] args)
---
*/

int main(lazy char[][] args)
{
    return args.length;
}
