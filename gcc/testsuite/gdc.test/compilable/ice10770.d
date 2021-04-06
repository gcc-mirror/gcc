enum E1 : int;
static assert(is(E1 e == enum) && is(e == int));

enum E2;
static assert(is(E2 e == enum));
