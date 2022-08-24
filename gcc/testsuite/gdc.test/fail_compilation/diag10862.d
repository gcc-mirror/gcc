/*
TEST_OUTPUT:
---
fail_compilation/diag10862.d(40): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(41): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(42): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(43): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(44): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(46): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(47): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(48): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(49): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(51): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(52): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(53): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(54): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(56): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(57): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(58): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(59): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(61): Error: undefined identifier `semanticError`
fail_compilation/diag10862.d(71): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(74): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d-mixin-77(77): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d-mixin-78(78): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d-mixin-79(79): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d-mixin-80(80): Error: using the result of a comma expression is not allowed
fail_compilation/diag10862.d-mixin-80(80): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d-mixin-83(83): Error: `a + b` is not an lvalue and cannot be modified
fail_compilation/diag10862.d-mixin-84(84): Error: undefined identifier `c`
fail_compilation/diag10862.d(86): Error: undefined identifier `semanticError`
fail_compilation/diag10862.d(93): Error: lazy variable `bar` cannot be modified
fail_compilation/diag10862.d(95): Error: template instance `diag10862.test3.foo!int` error instantiating
---
*/
void test1()
{
    int a, b;

    if (a = b) {}
    if ((a = b) = 0) {}
    if ((a = b) = (a = b)) {}
    if (a = 0, b = 0) {}        // https://issues.dlang.org/show_bug.cgi?id=15384
    if (auto x = a = b) {}      // this is error, today

    while (a = b) {}
    while ((a = b) = 0) {}
    while ((a = b) = (a = b)) {}
    while (a = 0, b = 0) {}     // https://issues.dlang.org/show_bug.cgi?id=15384

    do {} while (a = b);
    do {} while ((a = b) = 0);
    do {} while ((a = b) = (a = b));
    do {} while (a = 0, b = 0); // https://issues.dlang.org/show_bug.cgi?id=15384

    for (;  a = b; ) {}
    for (;  (a = b) = 0; ) {}
    for (;  (a = b) = (a = b); ) {}
    for (;  a = 0, b = 0; ) {}  // https://issues.dlang.org/show_bug.cgi?id=15384

    semanticError;
}

void test2()
{
    int a, b;

    // (a + b) cannot be an assignment target.
    // However checkAssignAsCondition specilatively rerites it to EqualExp,
    // then the pointless error "is not an lvalue" would not happen.
    if (a + b = a * b) {}

    // The suggestion error masks "undefined identifier" error
    if (a = undefinedIdentifier) {}

    // If the condition is a mixin expression
    if (mixin("a = b")) {}
    if (mixin("(a = b) = 0")) {}
    if (mixin("(a = b) = (a = b)")) {}
    if (mixin("a = 0, b = 0")) {}
    if (auto x = mixin("a = b")) {}     // Note: no error

    if (mixin("a + b = a * b")) {}      // Note: "a + b is not an lvalue"
    if (mixin("a = c")) {}

    semanticError;
}

void test3()
{
    void foo(T)(lazy T bar)
    {
        bar = 2;
    }
    foo(1 + 1);
}
