// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail_opover.d(39): Error: no `[]` operator overload for type `object.Object`
$p:object.d$(110):        `object.Object` declared here
fail_compilation/fail_opover.d(43): Error: no `[]` operator overload for type `TestS`
fail_compilation/fail_opover.d(41):        `fail_opover.test1.TestS` declared here
fail_compilation/fail_opover.d(55): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(56): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(57): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(58): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(59): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(60): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(61): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(62): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(63): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(64): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(65): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
fail_compilation/fail_opover.d(66): Error: no `[]` operator overload for type `S`
fail_compilation/fail_opover.d(48):        `fail_opover.test2.S` declared here
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
