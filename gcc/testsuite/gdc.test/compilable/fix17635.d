// https://issues.dlang.org/show_bug.cgi?id=17635

alias T = immutable int;

T** f(const T** input) pure
{
    T** output;
    return output;
}

void main()
{
    T i;
    T* p = &i;
    immutable T** r = f(&p);
}
