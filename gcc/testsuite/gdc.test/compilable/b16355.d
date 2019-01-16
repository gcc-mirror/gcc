// REQUIRED_ARGS: -c
struct S0 { this(this) {} }
struct S1 { S0[2] x; }
struct S2 { S0[0] x; }

// S0 has an explicit and a compiler-generated postblit
static assert( __traits(hasMember, S0, "__postblit"));
static assert( __traits(hasMember, S0, "__xpostblit"));
// S1 has only the compiler-generated postblit
static assert(!__traits(hasMember, S1, "__postblit"));
static assert( __traits(hasMember, S1, "__xpostblit"));
// S2 has no postblit at all since the x array has zero length
static assert(!__traits(hasMember, S2, "__postblit"));
static assert(!__traits(hasMember, S2, "__xpostblit"));
