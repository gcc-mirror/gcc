/* TEST_OUTPUT:
---
fail_compilation/fail_typeof.d(15): Error: undefined identifier `this`
fail_compilation/fail_typeof.d(20): Error: `this` is not in a class or struct scope
fail_compilation/fail_typeof.d(25): Error: undefined identifier `super`
fail_compilation/fail_typeof.d(30): Error: `super` is not in a class scope
fail_compilation/fail_typeof.d(37): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/fail_typeof.d(47): Error: undefined identifier `super`
fail_compilation/fail_typeof.d(52): Error: `super` is not in a class scope
fail_compilation/fail_typeof.d(60): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/fail_typeof.d(70): Error: undefined identifier `super`, did you mean `typeof(super)`?
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
