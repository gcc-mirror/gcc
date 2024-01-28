/*
TEST_OUTPUT:
----
fail_compilation/ice14907.d(14): Error: struct `ice14907.S(int v = S)` recursive template expansion
fail_compilation/ice14907.d(19):        while looking for match for `S!()`
fail_compilation/ice14907.d(15): Error: template `ice14907.f(int v = f)()` recursive template expansion
fail_compilation/ice14907.d(20):        while looking for match for `f!()`
fail_compilation/ice14907.d(15): Error: template `ice14907.f(int v = f)()` recursive template expansion
fail_compilation/ice14907.d(21): Error: template `f` is not callable using argument types `!()()`
fail_compilation/ice14907.d(15):        Candidate is: `f(int v = f)()`
----
*/

struct S(int v = S) {}
void f(int v = f)() {}

void main()
{
    S!() s;     // OK <- ICE
    f!()();     // OK <- ICE
    f();        // OK <- ICE
}
