/* PERMUTE_ARGS: -O
 */

// https://issues.dlang.org/show_bug.cgi?id=22104

struct S { int a1, a2, a3; }

version (none)
void throws2ndCall(ref S x);
else
{
void throws2ndCall(ref S x)
{
    __gshared bool b;
    if (b)
        throw new Exception("n == 1");
    b = true;
}
}

void main() { foo(); }

void foo()
{
    S[] arr = [S(), S()];
    size_t i;
    try
    {
        for (i = 0; i < 2; i++)
            throws2ndCall(arr[i]);
    }
    catch (Exception o)
    {
        //printf("Exception: i = %lu\n", i);
        assert(i == 1);  // this fails
    }
}
