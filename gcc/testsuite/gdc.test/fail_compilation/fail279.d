// Issue 2920 - recursive templates blow compiler stack
// template_16

template Template(int i)
{
    mixin Template!(i + 1);
}
mixin Template!(0);
