// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/imp15907.d
// PERMUTE_ARGS:
import imports.imp15907;

struct S
{
    private int a;
}

void test()
{
    process(S());
}

static assert(allMembers!S == ["a"]);
enum sz = __traits(getMember, imports.imp15907, "PrivateStruct").sizeof;
static assert(__traits(hasMember, imports.imp15907, "privateVar"));
typeof(__traits(getMember, PublicStruct, "S").init) s;
