/*
TEST_OUTPUT:
---
fail_compilation/ufcs.d(31): Error: no property `regularF` for `s` of type `S`
fail_compilation/ufcs.d(31):        the following error occured while looking for a UFCS match
fail_compilation/ufcs.d(31): Error: function `regularF` is not callable using argument types `(S)`
fail_compilation/ufcs.d(31):        expected 0 argument(s), not 1
fail_compilation/ufcs.d(39):        `ufcs.regularF()` declared here
fail_compilation/ufcs.d(32): Error: no property `templateF` for `s` of type `S`
fail_compilation/ufcs.d(32):        the following error occured while looking for a UFCS match
fail_compilation/ufcs.d(32): Error: template `templateF` is not callable using argument types `!()(S)`
fail_compilation/ufcs.d(40):        Candidate is: `templateF()()`
fail_compilation/ufcs.d(33): Error: no property `templateO` for `s` of type `S`
fail_compilation/ufcs.d(33):        the following error occured while looking for a UFCS match
fail_compilation/ufcs.d(33): Error: none of the overloads of template `ufcs.templateO` are callable using argument types `!()(S)`
fail_compilation/ufcs.d(42):        Candidates are: `templateO()(int x)`
fail_compilation/ufcs.d(43):                        `templateO()(float y)`
fail_compilation/ufcs.d(36): Error: no property `local` for `s` of type `ufcs.S`
fail_compilation/ufcs.d(35):        cannot call function `local` with UFCS because it is not declared at module scope
fail_compilation/ufcs.d(26):        struct `S` defined here
---
*/



struct S { }

void f()
{
    S s;
    s.regularF();
    s.templateF();
    s.templateO();

    void local(S) {}
    s.local();
}

void regularF();
void templateF()();

void templateO()(int x);
void templateO()(float y);
