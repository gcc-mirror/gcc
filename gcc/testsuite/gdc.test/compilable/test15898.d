// REQUIRED_ARGS: -O
// https://issues.dlang.org/show_bug.cgi?id=15898

int addAssignSimple(int[] , const(int)[] )
{
    uint c;
    return c;
}

void mulKaratsuba(int[] result, const(int)[] x, const(int)[] y, int[] )
{
    const(int)[] y1 = y;
    int[] newscratchbuff;
    int[] resultHigh = result;

    bool ysmaller2 = x.length >= y1.length;
    newscratchbuff[0..y1.length] = resultHigh;
    mulKaratsuba(
        resultHigh[1..$],
        ysmaller2 ? x[1..$] : y1,
        ysmaller2 ? y1 : x,
        newscratchbuff[y1.length..$]
    );

    addAssignSimple(resultHigh[1..$], newscratchbuff[0..y1.length]);
}

