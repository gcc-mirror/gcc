/*
Segfault on DMD 0.095
http://www.digitalmars.com/d/archives/digitalmars/D/bugs/926.html
*/
module test;

template Test()
{
    bool opIndex(bool x)
    {
        return !x;
    }
}

void main()
{
    mixin Test!() xs;
    bool x = xs[false];
}


