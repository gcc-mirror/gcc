/* DISABLED: win32 win64
REQUIRED_ARGS: -extern-std=c++11
*/

#line 100

// Make sure that bad things don't happen if the user isn't using
// `core.attribute`'s definition
struct gnuAbiTag
{
    string[] args;
}

extern(C++):

@gnuAbiTag(["42"])
struct A {}

__gshared A a;

static assert(a.mangleof == "a");
