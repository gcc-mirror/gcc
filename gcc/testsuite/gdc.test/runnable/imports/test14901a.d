module imports.test14901a;

//extern(C) int printf(const char*, ...);

extern extern(C) __gshared static int initCount;

int make(string s)()
{
    __gshared static int value;

    struct WithCtor
    {
        shared static this()
        {
            //printf("%s\n", s.ptr);
            initCount++;
        }
    }

    return value;
}
