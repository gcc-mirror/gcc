/* REQUIRED_ARGS: -betterC -inline
   PERMUTE_ARGS:
*/

struct InvBoneBindInfo
{
}


struct Test(Value)
{
    void test()
    {
        auto t = Value.init;
    }
}

extern(C) void main()
{
    Test!(InvBoneBindInfo[32]) test;
    test.test();
}
