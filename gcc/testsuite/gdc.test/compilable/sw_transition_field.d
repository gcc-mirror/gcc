// PERMUTE_ARGS:
// REQUIRED_ARGS: -transition=field
/*
TEST_OUTPUT:
---
compilable/sw_transition_field.d(15): `sw_transition_field.S1.ix` is `immutable` field
compilable/sw_transition_field.d(16): `sw_transition_field.S1.cx` is `const` field
compilable/sw_transition_field.d(21): `sw_transition_field.S2!(immutable(int)).S2.f` is `immutable` field
compilable/sw_transition_field.d(21): `sw_transition_field.S2!(const(int)).S2.f` is `const` field
---
*/

struct S1
{
    immutable int ix = 1;
    const int cx = 2;
}

struct S2(F)
{
    F f = F.init;
}

alias S2!(immutable int) S2I;
alias S2!(    const int) S2C;
