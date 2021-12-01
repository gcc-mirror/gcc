// https://issues.dlang.org/show_bug.cgi?id=21378

version(all)
    enum do_inline = true;

pragma(inline, do_inline)
void stuff(){}

void stuff2()
{
    pragma(inline, do_inline);
}

pragma(inline, canInline(1))
void stuff3(){}

void stuff4()
{
    pragma(inline, canInline(1));
}

void main()
{
    stuff();
    stuff2();
    stuff3();
    stuff4();
}

int canInline(int x)
{
    return x*x;
}
