// REQUIRED_ARGS: -O -inline

// https://issues.dlang.org/show_bug.cgi?id=17146

struct S { int[] a; int b; }

void foo()
{
    S[] s;
    if (s[$-1] == S.init) {}
}

void bar() { foo(); }
