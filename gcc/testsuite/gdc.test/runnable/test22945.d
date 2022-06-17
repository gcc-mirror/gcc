/*
REQUIRED_ARGS: -release -check=assert=on
PERMUTE_ARGS:  -check=invariant=on
*/

// https://issues.dlang.org/show_bug.cgi?id=22945

bool hitStruct;
bool hitClass;

struct S
{
    this(int) {}
    invariant { hitStruct = true; }
}

class C
{
    this() {}
    invariant { hitClass = true; }
}

int main()
{
    cast(void) S(0);
    cast(void) new C();

    version(D_Invariants)
    {
        assert(hitStruct && hitClass);
    }
    else
    {
        assert(!hitStruct && !hitClass);
    }

    return 0;
}
