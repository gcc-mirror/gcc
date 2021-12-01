/* REQUIRED_ARGS: -O
 * PERMUTE_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=18534

auto blah(char ch) { return ch; }

auto foo(int i)
{
    return blah(i ? 'A' : 'A');
}

void main()
{
    auto c = foo(0);
    assert(c == 'A');
}
