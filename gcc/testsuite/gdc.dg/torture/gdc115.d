// https://bugzilla.gdcproject.org/show_bug.cgi?id=115
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void main()
{
    union U
    {
        float f;
        uint i;
    }
    float a = 123.0;
    const l = U(a);

    assert(l.i == U(a).i);
}
