// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
AliasSeq!("id", "toString", "toHash", "opCmp", "opEquals", "Monitor", "factory")
genProps
---
*/

class User : Entity!User
{
    int id;
}

class Entity(T)
{
    pragma(msg, generateProperties!T);
    /* Compiler runs pragma(msg) in semantic() phase, but it does not insert any members
     * in this class. Therefore getting __traits(allMembers, User) while evaluating
     * generateProperties!User should work.
     */
}

template generateProperties(alias To)
{
    string getProperties(alias Ta)()
    {
        string toRet = "genProps";

        // This line is bad
        pragma(msg, __traits(allMembers, Ta));

        return toRet;
    }

    enum generateProperties = getProperties!(To);
}
