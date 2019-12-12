// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

template T(alias f) {}
static assert(!is(T!( (int x){ return invalid; } )));
