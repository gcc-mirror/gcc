/*
TEST_OUTPUT:
---
fail_compilation/templateoverload.d(17): Error: template instance `T!1` does not match any template declaration
fail_compilation/templateoverload.d(17):        Candidates are:
fail_compilation/templateoverload.d(14):        T(X)
fail_compilation/templateoverload.d(15):        T()
fail_compilation/templateoverload.d(22): Error: template instance `V!int` does not match any template declaration
fail_compilation/templateoverload.d(22):        Candidates are:
fail_compilation/templateoverload.d(19):        V(int i)
fail_compilation/templateoverload.d(20):        V(T, alias a)
---
*/
template T(X) {}
template T() {}

alias t = T!1;

template V(int i) {}
template V(T, alias a) {}

alias v = V!int;
