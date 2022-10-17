module ice21696;

// https://issues.dlang.org/show_bug.cgi?id=21696

double[1][1] func(double[1][1] stuff = [[1.0]])
{
    bool myFunc()
    {
        if (stuff[])
            return true;
        return false;
    }

    if (!myFunc())
        assert(false);

    return stuff;
}

int main()
{
    return func() != [[1.0]];
}
