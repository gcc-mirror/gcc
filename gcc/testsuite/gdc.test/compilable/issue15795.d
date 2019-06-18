module issue15795;

template Foo(T : int) {}
template Bar(T) {}
alias Bar = Foo;

alias bi = Bar!int;
alias fi = Foo!int;
static assert(__traits(isSame, bi, fi));
