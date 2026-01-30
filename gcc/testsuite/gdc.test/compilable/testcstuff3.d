// EXTRA_FILES: imports/cstuff3.c
import imports.cstuff3;

static assert(squared(4) == 16);

/* test case for issue #21094 */
string enum_to_str(E)(E v) if (is(E == enum))
{
    final switch (v) with(E)
    {
        static foreach (m; __traits(allMembers, E))
        {
    case mixin(m):
            return m;
        }
    }
}

void testEnumSwitch()
{
    auto str = enum_to_str(UPNG_EOK);
    assert(str == "UPNG_EOK");
}
