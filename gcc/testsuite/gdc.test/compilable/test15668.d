void foo ( int line = __LINE__ ) ( string msg = "" )
{
    static assert (line == 8);
}

void main()
{
    foo();
}
