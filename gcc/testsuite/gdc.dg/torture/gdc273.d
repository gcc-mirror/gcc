// https://bugzilla.gdcproject.org/show_bug.cgi?id=273
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

class B273
{
    B273[] members;
}

class D273 : B273
{
}

void main()
{
    auto noPointers = ClassInfo.ClassFlags.noPointers;
    assert((B273.classinfo.m_flags & noPointers) == 0);
    assert((D273.classinfo.m_flags & noPointers) == 0);
}
