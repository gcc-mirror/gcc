// REQUIRED_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/test19107.d(20): Error: template `test19107.all` cannot deduce function from argument types `!((c) => c)(string[])`, candidates are:
fail_compilation/test19107.d(14):        `test19107.all(alias pred, T)(T t) if (is(typeof(I!pred(t))))`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19107

import imports.test19107b;

void all(alias pred, T)(T t)
    if (is(typeof(I!pred(t))))
{ }

void main(string[] args)
{
    args.all!(c => c);
}
