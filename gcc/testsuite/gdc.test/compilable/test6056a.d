alias const(typeof('c')*) A;
alias const(typeof(0)*) B;
static assert(is(B == const(int*)));
