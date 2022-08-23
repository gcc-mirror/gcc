/* REQUIRED_ARGS:
 * OPTIONAL_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=17246

struct Foo
{
    int* rc;
    this(int val)
    {
        rc = new int;
        (*rc) = 1;
    }
    this(this)
    {
        (*rc)++;
    }
    ~this()
    {
        if (rc)
        {
            assert(*rc > 0);
            (*rc)--;
        }
    }
}

struct Bar
{
    Foo foo;
    this(Foo foo, bool)
    {
        this.foo = foo;
    }
}

bool fun(bool val) { return !val; }

auto genBar(bool flag)
{
    return flag ? Bar() : Bar(Foo(10), fun(!flag));
}

int main(string[] args)
{
    auto bar = genBar(args.length == 0);
    return 0;
}
