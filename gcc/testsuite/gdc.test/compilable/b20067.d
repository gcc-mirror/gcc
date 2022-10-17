struct S1
{
    int i;
    @property int ii(){return 0;}
    @property bool b(){return true;}
    alias empty = b;
    alias front = ii;
    void popFront(){}
}
struct S2
{
    int i;
    bool b;
    alias empty = b;
    alias front = i;
    void popFront(){}
}

void main()
{
    foreach(n; S1()) { } // 2.086: Error: cannot infer argument types
    foreach(n; S2()) { } // 2.086: Error: cannot infer argument types
}
