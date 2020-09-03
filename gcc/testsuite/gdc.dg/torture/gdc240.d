// https://bugzilla.gdcproject.org/show_bug.cgi?id=240
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void test240(int a, int b)
{
    assert(a == 0);
    assert(b == 0);
}

void main()
{
    int a = 0;
    test240(a, a++);
    assert(a == 1);
}
