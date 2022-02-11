// PERMUTE_ARGS:
// REQUIRED_ARGS: -transition=tls
/*
TEST_OUTPUT:
---
compilable/sw_transition_tls.d(11): `x` is thread local
compilable/sw_transition_tls.d(15): `y` is thread local
---
*/

int x;

struct S
{
    static int y;
}
