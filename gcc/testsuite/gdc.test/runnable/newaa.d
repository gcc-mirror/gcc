void main()
{
    alias AA = int[string];
    // aa is not ref
    static void test(AA aa)
    {
        aa[""] = 0;
    }
    auto aa = new AA();
    auto ab = new int[string];
    auto ac = new typeof(aa);
    test(aa);
    test(ab);
    test(ac);
    assert(aa.length);
    assert(ab.length);
    assert(ac.length);

    int[string] a = new int[string];
    auto b = a;
    a["seven"] = 7;
    assert(b["seven"] == 7);
}
