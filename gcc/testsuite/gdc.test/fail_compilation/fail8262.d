/* TEST_OUTPUT:
---
fail_compilation/fail8262.d(32): Error: initializer must be an expression, not `Tuple8262!1`
fail_compilation/fail8262.d(27): Error: template instance `fail8262.T8262!(Tuple8262!1)` error instantiating
fail_compilation/fail8262.d(19): Error: cannot implicitly convert expression `S(0)` of type `S` to `int`
---
 * https://issues.dlang.org/show_bug.cgi?id=8262
 */

template Seq(T...) { alias T Seq; }

struct S
{
    int s;
    alias Seq!s _;
    alias _ this;
}

int si = S.init;

struct Tuple8262(T...)
{
    alias T expand;
    alias expand this;
}

auto data = T8262!(Tuple8262!1);
//pragma(msg, data);

template T8262(T)
{
    immutable(int) T8262 = T;
}
