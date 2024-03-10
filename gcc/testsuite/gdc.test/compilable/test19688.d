/* TEST_OUTUT:
---
---
*/
void test(string s = __FUNCTION__ ~ __MODULE__ ~ __FUNCTION__)
{
    assert(s == "test19688.maintest19688test19688.main");
}

void main()
{
    test();
}
