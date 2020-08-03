// https://bugzilla.gdcproject.org/show_bug.cgi?id=194
// { dg-do compile }

auto test194(ref bool overflow)
{
    import core.checkedint;

    return adds(1, 1, overflow);
}
