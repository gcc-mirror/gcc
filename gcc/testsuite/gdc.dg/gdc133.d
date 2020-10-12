// https://bugzilla.gdcproject.org/show_bug.cgi?id=133
// { dg-do compile }

void delegate()[] D133;

void test133a(void delegate() dg)
{
    D133 ~= dg;
}

void test133()
{
    void nested()
    {}
    test133a(&nested);
}
