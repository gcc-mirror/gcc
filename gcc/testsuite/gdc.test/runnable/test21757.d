// https://github.com/dlang/dmd/issues/21757

struct S
{
    void*[300_000] arr; // generates stupidly large symbols for RTImfoImpl!() that crash MS link.exe
}

void main()
{
}
