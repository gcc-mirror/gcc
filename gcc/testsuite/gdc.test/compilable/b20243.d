module b20243;

inout(int) f(inout(int)[inout(string)] x);
const(int)[const(string)] x;
static assert(is(typeof(f(x)) == const(int)));

inout(int)[inout(string)] g(inout(int) y);
const(int) y;
static assert(is(typeof(g(y)) == const(int)[const(string)]));
