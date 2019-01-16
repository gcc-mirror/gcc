// REQUIRED_ARGS: -fPIC -lib
// PERMUTE_ARGS:
// DISABLED: win32 win64
extern void throwing();

void foo()
{
    // create plenty of symbols, so that the catch references get a high symbol index
    static int a0, a1, a2, a3, a4, a5, a6, a7, a8, a9,
        b0, b1, b2, b3, b4, b5, b6, b7, b8, b9,
        c0, c1, c2, c3, c4, c5, c6, c7, c8, c9,
        d0, d1, d2, d3, d4, d5, d6, d7, d8, d9;
    try
    {
        throwing();
    }
    catch (Exception)
    {
    }
}

void bar()
{
    try
    {
        throwing();
    }
    // symbol index for DW.ref._D9Exception7__ClassZ
    // gets reused for another object and is out of bounds
    catch (Exception)
    {
    }
}
