/*
TEST_OUTPUT:
---
fail_compilation/ufcs.d(25): Error: no property `regularF` for `s` of type `S`
fail_compilation/ufcs.d(25):        the following error occured while looking for a UFCS match
fail_compilation/ufcs.d(25): Error: function `ufcs.regularF()` is not callable using argument types `(S)`
fail_compilation/ufcs.d(25):        expected 0 argument(s), not 1
fail_compilation/ufcs.d(26): Error: no property `templateF` for `s` of type `S`
fail_compilation/ufcs.d(26):        the following error occured while looking for a UFCS match
fail_compilation/ufcs.d(26): Error: template `ufcs.templateF` is not callable using argument types `!()(S)`
fail_compilation/ufcs.d(31):        Candidate is: `templateF()()`
fail_compilation/ufcs.d(27): Error: no property `templateO` for `s` of type `S`
fail_compilation/ufcs.d(27):        the following error occured while looking for a UFCS match
fail_compilation/ufcs.d(27): Error: none of the overloads of template `ufcs.templateO` are callable using argument types `!()(S)`
fail_compilation/ufcs.d(33):        Candidates are: `templateO()(int x)`
fail_compilation/ufcs.d(34):                        `templateO()(float y)`
---
*/

struct S { }

void f()
{
    S s;
    s.regularF();
    s.templateF();
    s.templateO();
}

void regularF();
void templateF()();

void templateO()(int x);
void templateO()(float y);
