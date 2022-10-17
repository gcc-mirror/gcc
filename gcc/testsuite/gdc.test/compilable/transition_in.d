// REQUIRED_ARGS: -transition=in
/*
TRANSFORM_OUTPUT: remove_lines(druntime)
TEST_OUTPUT:
---
compilable/transition_in.d(3): Usage of 'in' on parameter
compilable/transition_in.d(3): Usage of 'in' on parameter
compilable/transition_in.d(8): Usage of 'in' on parameter
compilable/transition_in.d(13): Usage of 'in' on parameter
---
*/
#line 1
struct Foobar
{
    void bar (in int a, in Object c);
}

version (none)
{
    void barfoo (in string arg);
}

void main ()
{
    void nested (in char c) {}
}
