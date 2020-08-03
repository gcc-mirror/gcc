// https://bugzilla.gdcproject.org/show_bug.cgi?id=196
// { dg-do assemble }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

class C196
{
    int a;
}

struct S196
{
    int a;
}

void test196()
{
    __gshared c = new C196();
    __gshared s = new S196(0);
    c.a = 1;
    s.a = 1;
}
