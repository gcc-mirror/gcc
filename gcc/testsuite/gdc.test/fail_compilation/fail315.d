/*
TEST_OUTPUT:
---
fail_compilation/fail315.d-mixin-17(17): Error: found `;` when expecting `,`
fail_compilation/fail315.d-mixin-17(17): Error: expression expected, not `}`
fail_compilation/fail315.d-mixin-17(17): Error: found `End of File` when expecting `,`
fail_compilation/fail315.d-mixin-17(17): Error: found `End of File` when expecting `]`
fail_compilation/fail315.d-mixin-17(17): Error: found `End of File` when expecting `;` following `return` statement
fail_compilation/fail315.d-mixin-17(17): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/fail315.d-mixin-17(17):        unmatched `{`
fail_compilation/fail315.d(22): Error: template instance `fail315.foo!()` error instantiating
---
*/

void foo(S...)(S u)
{
    alias typeof(mixin("{ return a[1;}()")) z;
}

void main()
{
    foo!()(0);
}
