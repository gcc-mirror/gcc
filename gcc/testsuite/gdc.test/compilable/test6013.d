// REQUIRED_ARGS: -de
import imports.test6013;

static assert(__traits(compiles, public_alias_value));
static assert(!__traits(compiles, private_alias_value));
static assert(__traits(compiles, public_alias_func()));
static assert(!__traits(compiles, private_alias_func()));
static assert(__traits(compiles, () { public_alias_type val; }));
static assert(!__traits(compiles, () { private_alias_type val; }));
