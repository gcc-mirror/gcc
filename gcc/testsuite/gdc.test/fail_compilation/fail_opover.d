// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail_opover.d(13): Error: no `[]` operator overload for type `object.Object`
fail_compilation/fail_opover.d(17): Error: no `[]` operator overload for type `TestS`
---
*/
void test1()
{
    Object m;
    m[] = error;

    struct TestS {}
    TestS s;
    s[] = error;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_opover.d(46): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(47): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(49): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(50): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(51): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(52): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(53): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(54): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(55): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(56): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(57): Error: no `[]` operator overload for type `S`
---
*/
void test2()
{
    struct S
    {
        void func(int) {}
        alias func this;
    }
    S s;
    // The errors failing aliasthis access need to be gagged for better error messages.
    s[];            // in ArrayExp::op_overload()
    s[1];           // ditto
    s[1..2];        // ditto
    +s[];           // in UnaExp::op_overload()
    +s[1];          // ditto
    +s[1..2];       // ditto
    s[] = 3;        // in AssignExp::semantic()
    s[1] = 3;       // ditto
    s[1..2] = 3;    // ditto
    s[] += 3;       // in BinAssignExp::op_overload()
    s[1] += 3;      // ditto
    s[1..2] += 3;   // ditto
}
