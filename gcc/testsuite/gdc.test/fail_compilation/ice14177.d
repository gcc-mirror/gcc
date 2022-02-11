/*
TEST_OUTPUT:
----
fail_compilation/ice14177.d(8): Error: alias `ice14177.Primitive` recursive alias declaration
----
*/

alias Primitive = Atom*;
alias Atom = Primitive;

void main()
{
    Atom atom;
    atom;
}
