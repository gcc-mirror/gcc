// https://issues.dlang.org/show_bug.cgi?id=23951

struct S { int x; }
struct T { S a; alias a this; }
struct U { T t; }
static assert(__traits(hasMember, T, "x"));
static assert(__traits(hasMember, T.init, "x"));
static assert(__traits(hasMember, U.init.t, "x"));
static assert(__traits(hasMember, U.t, "a"));
static assert(__traits(hasMember, U.t, "x"));
