enum A { B }
static assert(is(typeof(A.B) == A));
static assert(is(typeof(A(A.B)) == A));
