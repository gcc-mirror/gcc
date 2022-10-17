/*
TEST_OUTPUT:
---
fail_compilation/fail19209.d(16): Error: function `fail19209.Spammer.method()` does not override any function, did you mean to override variable `fail19209.Spam.method`?
fail_compilation/fail19209.d(16):        Functions are the only declarations that may be overriden
---
*/

class Spam
{
    int method;
}

class Spammer : Spam
{
    override method() {}
}
