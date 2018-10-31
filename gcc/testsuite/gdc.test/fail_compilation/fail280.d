// Issue 2920 - recursive templates blow compiler stack
// template_17_A.

template t(int i)
{
    const int x = t!(i + 1).x;
}

void main()
{
    int i = t!(0).x;
}
