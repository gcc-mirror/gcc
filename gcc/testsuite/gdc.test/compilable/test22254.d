// https://issues.dlang.org/show_bug.cgi?id=22254

struct Template(T) { T t; }

Template!Bar a;
Template!Bar b;

immutable struct Bar { }

static assert(is(typeof(a) == typeof(b)));
static assert(is(typeof(a) == Template!(immutable Bar)));

Template!C c1;
Template!C c2;

immutable class C { }

static assert(is(typeof(c1) == typeof(c2)));
static assert(is(typeof(c1) == Template!(immutable C)));
