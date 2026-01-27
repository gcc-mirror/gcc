// { dg-do run }
// { dg-additional-options "-fpreview=dip1000" }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
struct S
{
    this(int p) nothrow @nogc @safe
    { f = p; }
    int f;
}

int main() nothrow @nogc @safe
{
    scope S[] sa = [S(1), S(2)];

    assert(sa[0].f == 1);
    assert(sa[1].f == 2);
    return 0;
}
