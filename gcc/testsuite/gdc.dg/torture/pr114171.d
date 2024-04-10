// { dg-do run }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.builtins;

struct S1
{
    string label;
}

struct S2
{
    ulong pad;
    S1 label;
}

pragma(inline, false)
auto newitem()
{
    void *p = __builtin_malloc(S2.sizeof);
    __builtin_memset(p, 0, S2.sizeof);
    return cast(S2*) p;
}

int main()
{
    auto bn = newitem();
    return bn.label is S1.init ? 0 : 1;
}
