// PERMUTE_ARGS: -O

// https://issues.dlang.org/show_bug.cgi?id=17940

struct Array
{
    long length;
    long ptr;
}

struct Struct
{
    bool b = true;
}

void fun1(int)
{
}

void fun2(Array arr, int, int)
{
    assert(!arr.length);
}

void fn(Struct* str)
{
    Array arr;
    if (!str)
    {
        return;
    }
    if (str)
    {
        fun1(str.b);
    }
    if (str.b)
    {
        fun2(arr, str.b, 0);
    }
}

void main()
{
    Struct s;
    fn(&s);
}
