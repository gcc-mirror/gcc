// REQUIRED_ARGS: -de
// https://issues.dlang.org/show_bug.cgi?id=18647
deprecated void main ()
{
    Object o = new Object;
    delete o;
}
