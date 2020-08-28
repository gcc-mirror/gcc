// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96157
// { dg-options "-fno-moduleinfo -fno-rtti" }
// { dg-do compile }

int[] testYearsBC;

struct FilterResult
{
    int[] input;
    bool primed;

    this(int[] r)
    {
        this.input = r;
    }

    int front()
    {
        return input[0];
    }
};

FilterResult filter(int[] range)
{
    return FilterResult(range);
}

int[] chain(int[] rs)
{
    return rs;
}

struct SysTime
{
    this(int);
}

void test()
{
    while (1)
    {
        FilterResult val = filter(chain(testYearsBC));
        int year = val.front();
        SysTime(0);
    }
}
