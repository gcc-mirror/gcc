// REQUIRED_ARGS: -preview=dip1000

// https://issues.dlang.org/show_bug.cgi?id=22916
// Issue 22916 - [dip1000] copy of ref return still treated as scope variable (edit)

@safe:
struct Arr
{
    int** ptr;
    ref int* index() return scope { return *ptr; }
    void assign(int* p) scope { *ptr = p; }
}

void main0()
{
    scope Arr a;
    a.assign(a.index());
}

// https://issues.dlang.org/show_bug.cgi?id=23682
ref char* front_p(ref return scope char** p) { return *p; }
ref char* front_r(    return scope char** p) { return *p; }

char* g;

void test23862()
{
    scope char** _errors;
    g = front_p(_errors);   // should pass
    g = front_r(_errors);   // should pass
}

// Test case reduced from druntime
ref int* monitor(return scope Object h) pure nothrow @nogc @trusted
{
    return *cast(int**)&h.__monitor;
}

int* getMonitor(Object h) pure @nogc
{
    return monitor(h); // should pass
}
