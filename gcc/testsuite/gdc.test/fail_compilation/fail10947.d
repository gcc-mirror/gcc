/*
TEST_OUTPUT:
---
fail_compilation/fail10947.d(21): Error: cannot have immutable out parameter of type immutable(S)
fail_compilation/fail10947.d(22): Error: cannot have immutable out parameter of type immutable(S)
fail_compilation/fail10947.d(23): Error: cannot have immutable out parameter of type immutable(S)
fail_compilation/fail10947.d(25): Error: cannot have const out parameter of type const(S)
fail_compilation/fail10947.d(26): Error: cannot have const out parameter of type const(S)
fail_compilation/fail10947.d(27): Error: cannot have const out parameter of type const(S)
fail_compilation/fail10947.d(29): Error: cannot have inout out parameter of type inout(S)
fail_compilation/fail10947.d(30): Error: cannot have inout out parameter of type inout(S)
fail_compilation/fail10947.d(31): Error: cannot have inout out parameter of type inout(S)
---
*/

struct S {}
alias SI = immutable S;
alias SC = const S;
alias SW = inout S;

void fooi1(out SI) {}
void fooi2(out immutable(S)) {}
void fooi3(out immutable S) {}

void fooc1(out SC) {}
void fooc2(out const(S)) {}
void fooc3(out const S) {}

void foow1(out SW) {}
void foow2(out inout(S)) {}
void foow3(out inout S) {}
