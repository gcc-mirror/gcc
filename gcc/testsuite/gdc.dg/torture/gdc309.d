// https://bugzilla.gdcproject.org/show_bug.cgi?id=309
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void main()
{
    creal f1 = +0.0 + 0.0i;
    creal f2 = +0.0 - 0.0i;
    creal f3 = -0.0 + 0.0i;
    creal f4 = +0.0 + 0.0i;

    assert(f1 !is f2);
    assert(f1 !is f3);
    assert(f2 !is f3);
    assert(f1 is f4);

    assert(!(f1 is f2));
    assert(!(f1 is f3));
    assert(!(f2 is f3));
    assert(!(f1 !is f4));

    struct CReal
    {
        creal value;
    }

    CReal s1 = CReal(+0.0 + 0.0i);
    CReal s2 = CReal(+0.0 - 0.0i);
    CReal s3 = CReal(-0.0 + 0.0i);
    CReal s4 = CReal(+0.0 + 0.0i);

    assert(s1 !is s2);
    assert(s1 !is s3);
    assert(s2 !is s3);
    assert(s1 is s4);

    assert(!(s1 is s2));
    assert(!(s1 is s3));
    assert(!(s2 is s3));
    assert(!(s1 !is s4));
}
