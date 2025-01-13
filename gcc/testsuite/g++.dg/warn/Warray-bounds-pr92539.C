// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Warray-bounds" }

static bool
ischar(int ch)
{
    return (0 == (ch & ~0xff) || ~0 == (ch | 0xff)) != 0;
}

static bool eat(char const*& first, char const* last)
{
    if (first != last && ischar(*first)) { // { dg-bogus "bounds" }
        ++first;
        return true;
    }
    return false;
}

static bool eat_two(char const*& first, char const* last)
{
    auto save = first;
    if (eat(first, last) && eat(first, last))
        return true;
    first = save;
    return false;
}

static bool foo(char const*& first, char const* last)
{
    auto local_iterator = first;
    int i = 0;
    for (; i < 3; ++i)
        if (!eat_two(local_iterator, last))
            return false;
    first = local_iterator;
    return true;
}

static bool test(char const* in, bool full_match = true)
{
    auto last = in;
    while (*last)
        ++last;
    return foo(in, last) && (!full_match || (in == last)); // { dg-bogus "bounds" }
}

int main()
{
    return test("aa");
}

