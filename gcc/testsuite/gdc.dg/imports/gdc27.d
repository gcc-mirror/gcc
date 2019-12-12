module imports.gdc27;

interface I_A
{
    bool a();
}

class C_A : I_A
{
    bool a()
    {
        return false;
    }
}
