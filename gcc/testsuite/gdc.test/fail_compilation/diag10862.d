/*
TEST_OUTPUT:
---
fail_compilation/diag10862.d(28): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(29): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(30): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(31): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(32): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(34): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(35): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(36): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(37): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(39): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(40): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(41): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(42): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(44): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(45): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(46): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(47): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(49): Error: undefined identifier `semanticError`
---
*/
void test1()
{
    int a, b;

    if (a = b) {}
    if ((a = b) = 0) {}
    if ((a = b) = (a = b)) {}
    if (a = 0, b = 0) {}        // Bugzilla 15384
    if (auto x = a = b) {}      // this is error, today

    while (a = b) {}
    while ((a = b) = 0) {}
    while ((a = b) = (a = b)) {}
    while (a = 0, b = 0) {}     // Bugzilla 15384

    do {} while (a = b);
    do {} while ((a = b) = 0);
    do {} while ((a = b) = (a = b));
    do {} while (a = 0, b = 0); // Bugzilla 15384

    for (;  a = b; ) {}
    for (;  (a = b) = 0; ) {}
    for (;  (a = b) = (a = b); ) {}
    for (;  a = 0, b = 0; ) {}  // Bugzilla 15384

    semanticError;
}

/*
TEST_OUTPUT:
---
fail_compilation/diag10862.d(74): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d(77): Error: assignment cannot be used as a condition, perhaps `==` was meant?
fail_compilation/diag10862.d-mixin-80(80): Error: assignment cannot be used as a condition, perhaps == was meant?
fail_compilation/diag10862.d-mixin-81(81): Error: assignment cannot be used as a condition, perhaps == was meant?
fail_compilation/diag10862.d-mixin-82(82): Error: assignment cannot be used as a condition, perhaps == was meant?
fail_compilation/diag10862.d-mixin-83(83): Deprecation: Using the result of a comma expression is deprecated
fail_compilation/diag10862.d-mixin-83(83): Error: assignment cannot be used as a condition, perhaps == was meant?
fail_compilation/diag10862.d-mixin-86(86): Error: a + b is not an lvalue
fail_compilation/diag10862.d-mixin-87(87): Error: undefined identifier `c`
fail_compilation/diag10862.d(89): Error: undefined identifier `semanticError`
---
*/
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
