module imports.test17181a;

int a = 0;
static this() { a = 1; }

T abc(T)(T i)
{
    import imports.test17181b;
    return i;
}
