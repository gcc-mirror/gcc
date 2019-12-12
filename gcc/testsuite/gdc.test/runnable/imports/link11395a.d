module imports.link11395a;

struct SA
{
    bool flag;
    int[] nums;

    bool opEquals(bool b) { return flag == b; }
}

struct SB
{
    int num;
    SA sa;
}
