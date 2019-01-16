module imports.fail5385;

class C
{
    static private int privX;
    static package int packX;
    __gshared private int privX2;
    __gshared package int packX2;
}

struct S
{
    static private int privX;
    static package int packX;
    __gshared private int privX2;
    __gshared package int packX2;
}
