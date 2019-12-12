template X(T) { alias T X; }

alias const(X!char*) A;
alias const(X!int*) B;
static assert(is(B == const(int*)));
