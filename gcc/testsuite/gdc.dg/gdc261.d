// https://bugzilla.gdcproject.org/show_bug.cgi?id=261
// { dg-do compile }

void test261()
{
    class C1
    {
        void f1()
        {
            class C2
            {
                void f2()
                {
                    auto v = &f1;
                }
            }
        }
    }
}
