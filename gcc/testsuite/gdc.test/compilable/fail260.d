// PERMUTE_ARGS:
// REQUIRED_ARGS: -d

struct Static(uint width2, uint height2)
{
    immutable width = width2;
    immutable height = height2;

    static Static opCall()
    {
        Static ret;
        return ret;
    }

    alias float E;

    template MultReturn(alias M1, alias M2)
    {
        alias Static!(M2.width, M1.height) MultReturn;
    }

    void opMultVectors(M2)(M2 b)
    {
        alias MultReturn!(Static, M2) ret_matrix;
    }
}

void test()
{
    alias Static!(4, 1) matrix_stat;
    static matrix_stat m4 = matrix_stat();

    alias Static!(1, 4) matrix_stat2;
    static m6 = matrix_stat2();

    m6.opMultVectors(m4);
}
