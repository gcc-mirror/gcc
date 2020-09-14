// https://bugzilla.gdcproject.org/show_bug.cgi?id=51
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S51
{
    int x;
    int pad;

    this(this)
    {
        ++x;
    }
}

void main()
{
    S51 s;
    auto sarr = new S51[1];
    auto sarr2 = sarr;

    // postblit all fields.
    sarr2 ~= s;

    assert (sarr2[0].x == 1);
    assert (sarr2[1].x == 1);
    assert (sarr[0].x == 0);
    assert (s.x == 0);
}
