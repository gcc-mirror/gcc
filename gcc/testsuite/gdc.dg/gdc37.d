// https://bugzilla.gdcproject.org/show_bug.cgi?id=37
// { dg-do compile }

struct S37
{
    int bar(const S37 s)
    {
        return 0;
    }
}

int test37()
{
    S37 s;
    return s.bar(s);
}
