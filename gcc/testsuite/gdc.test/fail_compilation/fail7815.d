// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
X: tuple("x")
fail_compilation/fail7815.d(47): Error: no property 'flags' for type 'Move'
---
*/

mixin template Helpers()
{
    static if (is(Flags!Move))
    {
        Flags!Move flags;
    }
    else
    {
        pragma(msg, "X: ", __traits(derivedMembers, Flags!Move));
    }
}

template Flags(T)
{
    mixin({
        int defs = 1;
        foreach (name; __traits(derivedMembers, Move))
        {
            defs++;
        }
        if (defs)
        {
            return "struct Flags { bool x; }";
        }
        else
        {
            return "";
        }
    }());
}

struct Move
{
    int a;
    mixin Helpers!();
}

enum a7815 = Move.init.flags;

/+
This is an invalid case.
When the Move struct member is analyzed:
1. mixin Helpers!() is instantiated.
2. In Helpers!(), static if and its condition is(Flags!Move)) evaluated.
3. In Flags!Move, string mixin evaluates and CTFE lambda.
4. __traits(derivedMembers, Move) tries to see the member of Move.
   4a. mixin Helpers!() member is analyzed.
   4b. `static if (is(Flags!Move))` in Helpers!() is evaluated
   4c. The Flags!Move instantiation is already in progress, so it cannot be resolved.
   4d. `static if` fails because Flags!Move cannot be determined as a type.
5. __traits(derivedMembers, Move) returns a 1-length tuple("a").
6. The lambda in Flags!Move returns a string "struct Flags {...}", then
   Flags!Move is instantiated to a new struct Flags.
7. Finally Move struct does not have flags member, then the `enum a7815`
   definition will fail in its initializer.
+/
