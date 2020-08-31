// https://bugzilla.gdcproject.org/show_bug.cgi?id=141
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

bool test141(int a)
{
    return a > (a + 1);
}

void main()
{
    assert(test141(int.min) == false);
    assert(test141(int.max) == true);
}
