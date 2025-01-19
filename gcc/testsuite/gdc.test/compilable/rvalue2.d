/* PERMUTE_ARGS: -preview=rvaluerefparam
 */

struct S
{
    alias TT this;
    long TT();
    this(T)(int x) {}   // works if `int` is `long`

    this(S);
    this(ref S);

    ~this();
}

S fun(ref S arg);

void test()
{
    S st;
    fun(st);
}
