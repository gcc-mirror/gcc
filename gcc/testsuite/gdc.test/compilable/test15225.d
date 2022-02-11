// https://issues.dlang.org/show_bug.cgi?id=15225

alias foo = (int x)    => x + 1;
alias foo = (string x) => x ~ x;
alias foo = (float x)  => x - x;

static assert(   foo(1) == 2   );
static assert( foo("a") == "aa");
static assert(foo(2.0f) == 0.0f);
