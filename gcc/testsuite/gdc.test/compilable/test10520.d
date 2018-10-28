// REQUIRED_ARGS: -debug -profile

// Issue 10520 [profile+nothrow] Building with profiler results in "is not nothrow" error on some contracts

void f() { }

void g()()
in { f(); } // OK <- Error: 'main.f' is not nothrow
body { }

alias gi = g!();
