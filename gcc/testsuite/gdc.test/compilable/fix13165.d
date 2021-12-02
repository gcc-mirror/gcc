/* REQUIRED_ARGS: -w -profile
 */

// https://issues.dlang.org/show_bug.cgi?id=13165

void main()
{
    int i;
    if (!i)
        throw new Exception("Error");
    assert(0);
}
