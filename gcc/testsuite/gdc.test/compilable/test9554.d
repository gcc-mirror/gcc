// REQUIRED_ARGS: -o-

module pkg.test9554;
alias mod = pkg.test9554;

template Test(alias name) { enum Test = name; }
void fun() {}

static assert(fun.stringof == Test!(fun.stringof));
static assert(fun.stringof == "fun()");
static assert(fun.mangleof == Test!(fun.mangleof));
static assert(fun.mangleof == "_D3pkg8test95543funFZv");

static assert(mod.stringof == Test!(mod.stringof));
static assert(mod.stringof == "module test9554");
static assert(mod.mangleof == Test!(mod.mangleof));
static assert(mod.mangleof == "3pkg8test9554");

static assert(pkg.stringof == Test!(pkg.stringof));
static assert(pkg.stringof == "package pkg");
static assert(pkg.mangleof == Test!(pkg.mangleof));
static assert(pkg.mangleof == "3pkg");
