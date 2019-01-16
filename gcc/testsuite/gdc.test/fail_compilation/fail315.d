/*
TEST_OUTPUT:
---
fail_compilation/fail315.d-mixin-16(16): Error: found `;` when expecting `,`
fail_compilation/fail315.d-mixin-16(16): Error: expression expected, not `}`
fail_compilation/fail315.d-mixin-16(16): Error: found `EOF` when expecting `,`
fail_compilation/fail315.d-mixin-16(16): Error: found `EOF` when expecting `]`
fail_compilation/fail315.d-mixin-16(16): Error: found `EOF` when expecting `;` following return statement
fail_compilation/fail315.d-mixin-16(16): Error: found `EOF` when expecting `}` following compound statement
fail_compilation/fail315.d(21): Error: template instance fail315.foo!() error instantiating
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
