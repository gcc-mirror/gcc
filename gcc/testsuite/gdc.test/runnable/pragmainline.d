// REQUIRED_ARGS: -wi
// EXTRA_FILES: imports/pragmainline_a.d
/* TEST_OUTPUT:
---
---
*/


import imports.pragmainline_a;

auto anonclass()
{
    return new class {
        pragma(inline, true)
        final size_t foo()
        {
            return value();
        }
    };
}

auto testAlwaysInline()
{
    size_t var;

    foreach (d; Data("string"))
    {
        var = d.length();
    }

    assert(var == 6);

    var = anonclass().foo();

    assert(var == 10);

    auto nested = (size_t i) {
        return i - value();
    };

    var = nested(var);

    assert(var == 0);
}

void main()
{
    immutable baz = () => 1;
    assert(foo() == bar()());
    assert(foo() == baz());
    assert(bar()() == baz());

    testAlwaysInline();
}
