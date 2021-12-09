class ByNameC(alias Var)
{
    alias var = Var;
}

struct ByNameS(alias Var)
{
    alias var = Var;
    ubyte value = 1;
}

void main()
{
    ulong x;
    ByNameS!x v;
    ubyte w = 2;

    v.var = 0xAA_BB; /* stomps over v.value and w */

    assert(w == 2);
    assert(v.value == 1);
    //printf("%x\n", w); /* prints "aa", should be 2 */
    //printf("%x\n", v.value); /* prints "bb", should be 1 */

    auto c = new ByNameC!x;
    c.var = 0xAA_BB; /* stomps over c.__vptr */

    assert(*cast(ulong*)c != 0xAA_BB);
    //printf("%x\n", *cast(ulong*)c); /* prints "aabb", should be pointer value */

    assert(x == 0xAA_BB);
    //printf("%lx\n", x); /* prints 0, should be "aabb" */
}
