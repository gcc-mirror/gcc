// function type aliases
module issue16020;

alias F1 = const(int)(); const(int) f1(){return 42;}
static assert (is(F1 == typeof(f1)));

alias F2 = float(float); float f2(float p){return p;}
static assert (is(F2 == typeof(f2)));

alias F3 = void(); void f3(){}
static assert (is(F3 == typeof(f3)));

alias void F41() @safe;
alias F42 = void() @safe;
alias F43 = @safe void();
static assert (is(F41 == F42));
static assert (is(F43 == F42));

alias void F51() @system;
alias F52 = void() @safe;
static assert (!is(F51 == F52));

alias F61 = int() const shared;
alias int F62() const shared ;
alias F63 = const shared int();
static assert (is(F61 == F62));
static assert (is(F63 == F62));

alias F71 = int() immutable inout;
alias int F72() immutable inout;
alias F73 = immutable inout int();
static assert (is(F71 == F72));
static assert (is(F73 == F72));

alias FunTemplate(T) = void(T t);
alias Specialized = FunTemplate!int;
alias Compared = void(int);
static assert(is(Specialized == Compared));

// type suffixes
alias FT = const(int)*();
static assert(is(FT* == const(int)* function()));
alias FT2 = int*[2]() pure;
static assert(is(FT2* == int*[2] function() pure));
