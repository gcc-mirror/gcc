// https://github.com/dlang/dmd/issues/21435

void check_align()
{
    pragma(inline, false);
    int x;
    (cast(size_t)&x & 3) == 0 || assert(false);
}

char* getenv_setargv(char* env)
{
    auto p = env;
    auto q = env;
    auto r = env;
    auto s = env;
    auto t = env;
    auto u = env;
    auto ptr = env;

    while (1)
    {
        auto c = *ptr++;
        switch (c)
        {
            case 0:
                check_align();
                return env;

            case 'a':
                *p++ = 'A';
                continue;

            case 'b':
                *q++ = 'B';
                continue;

            case 'c':
                *r++ = 'C';
                continue;

            case 'd':
                *s++ = 'D';
                continue;

            case 'e':
                *t++ = 'E';
                continue;

            case 'f':
                *u++ = 'F';
                continue;

            case 'g':
                *p++ = 'G';
                continue;

            case 'h':
                *p++ = 'H';
                continue;

            default:
                *p++ = c;
                continue;
        }
    }
}

void main()
{
    char[10] m = "abcdefghi";
    auto str = getenv_setargv(m.ptr);
    str[0] || assert(false);
}
