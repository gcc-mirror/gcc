// REQUIRED_ARGS: -g

// https://issues.dlang.org/show_bug.cgi?id=104
// fails only with -g

Foofunc f;
alias int Foo;
alias int function(Foo) Foofunc;
