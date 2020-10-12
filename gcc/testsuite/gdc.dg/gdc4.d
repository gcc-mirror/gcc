// https://bugzilla.gdcproject.org/show_bug.cgi?id=4
// { dg-do compile }

void test4()
{
    string str = "allo";
    static assert(!__traits(compiles, str.reverse));
    static assert(!__traits(compiles, str.sort));
}
