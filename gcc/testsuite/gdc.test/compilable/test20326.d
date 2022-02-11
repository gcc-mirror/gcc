class C;
static assert(C.stringof == "C");

interface I;
static assert(I.stringof == "I");

union U;
static assert(U.stringof == "U");

struct S;
static assert(S.stringof == "S");
