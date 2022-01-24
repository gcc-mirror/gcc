// https://issues.dlang.org/show_bug.cgi?id=20410

enum E : string { foo = "foo" }

static assert( is(E  : string));
static assert( is(E  : T[], T));
static assert(!is(E == string));
static assert(!is(E == T[], T));
