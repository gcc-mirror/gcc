/* REQUIRED_ARGS:
 * PERMUTE_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=18737

struct S
{
    this(char);

    this(int j)
    {
        this('a');
        assert(0);
        this('b');
    }

    this(long j)
    {
        if (j)
        {
            this('c');
            assert(0);
        }
        else if (j + 1)
        {
            this('d');
            return;
        }
        this('e');
    }
}
