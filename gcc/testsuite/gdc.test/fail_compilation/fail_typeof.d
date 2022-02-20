/* TEST_OUTPUT:
---
fail_compilation/fail_typeof.d(18): Error: undefined identifier `this`
fail_compilation/fail_typeof.d(23): Error: `this` is not in a class or struct scope
fail_compilation/fail_typeof.d(23): Error: `this` is only defined in non-static member functions, not `fail_typeof`
fail_compilation/fail_typeof.d(28): Error: undefined identifier `super`
fail_compilation/fail_typeof.d(33): Error: `super` is not in a class scope
fail_compilation/fail_typeof.d(33): Error: `super` is only allowed in non-static class member functions
fail_compilation/fail_typeof.d(40): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/fail_typeof.d(50): Error: undefined identifier `super`
fail_compilation/fail_typeof.d(55): Error: `super` is not in a class scope
fail_compilation/fail_typeof.d(55): Error: `super` is only allowed in non-static class member functions
fail_compilation/fail_typeof.d(63): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/fail_typeof.d(73): Error: undefined identifier `super`, did you mean `typeof(super)`?
---
*/

enum E1 : this
{
    fail,
}

enum E2 : typeof(this)
{
    fail,
}

enum E3 : super
{
    fail,
}

enum E4 : typeof(super)
{
    fail,
}

struct S1
{
    enum E1 : this
    {
        fail,
    }

    enum E2 : typeof(this)
    {
        ok = S1(),
    }

    enum E3 : super
    {
        fail,
    }

    enum E4 : typeof(super)
    {
        fail,
    }
}

class C1
{
    enum E1 : this
    {
        fail,
    }

    enum E2 : typeof(this)
    {
        ok = new C1,
    }

    enum E3 : super
    {
        fail,
    }

    enum E4 : typeof(super)
    {
        ok = new C1,
    }
}
