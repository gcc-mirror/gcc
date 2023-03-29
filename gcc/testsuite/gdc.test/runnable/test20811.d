// https://issues.dlang.org/show_bug.cgi?id=20811

// OK: Mutable array literals are copied before CTFE takes ownership.
string issue20811a()
{
    char[1] counter = ['0'];
    counter[$-1]++;
    return counter.dup;
}

static assert(issue20811a() == "1");
static assert(issue20811a() == "1");
static assert(issue20811a() == "1");
static assert(issue20811a() == "1");

// Issue 20811: String literals were assumed to be read-only, so weren't copied.
string issue20811b()
{
    char[1] counter = "0";
    counter[$-1]++;
    return counter.dup;
}

static assert(issue20811b() == "1");
static assert(issue20811b() == "1");
static assert(issue20811b() == "1");
static assert(issue20811b() == "1");

void main()
{
    // Ensure CTFE did not overwrite the original AST.
    assert(issue20811a() == "1");
    assert(issue20811b() == "1");
}
