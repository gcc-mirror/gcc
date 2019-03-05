// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

void foo() {}
alias F = typeof(foo);

const        { void block_c() {} }
immutable    { void block_i() {} }
inout        { void block_w() {} }
shared       { void block_s() {} }
shared const { void block_sc() {} }
shared inout { void block_sw() {} }

static assert(is(typeof(block_c) == F));
static assert(is(typeof(block_i) == F));
static assert(is(typeof(block_w) == F));
static assert(is(typeof(block_s) == F));
static assert(is(typeof(block_sc) == F));
static assert(is(typeof(block_sw) == F));

version (all) { const:        void label_c() {} }
version (all) { immutable:    void label_i() {} }
version (all) { inout:        void label_w() {} }
version (all) { shared:       void label_s() {} }
version (all) { shared const: void label_sc() {} }
version (all) { shared inout: void label_sw() {} }

static assert(is(typeof(label_c) == F));
static assert(is(typeof(label_i) == F));
static assert(is(typeof(label_w) == F));
static assert(is(typeof(label_s) == F));
static assert(is(typeof(label_sc) == F));
static assert(is(typeof(label_sw) == F));

class C
{
    const        { static void block_c() {} }
    immutable    { static void block_i() {} }
    inout        { static void block_w() {} }
    shared       { static void block_s() {} }
    shared const { static void block_sc() {} }
    shared inout { static void block_sw() {} }

    static assert(is(typeof(block_c) == F));
    static assert(is(typeof(block_i) == F));
    static assert(is(typeof(block_w) == F));
    static assert(is(typeof(block_s) == F));
    static assert(is(typeof(block_sc) == F));
    static assert(is(typeof(block_sw) == F));

    version (all) { const:        static void label_c() {} }
    version (all) { immutable:    static void label_i() {} }
    version (all) { inout:        static void label_w() {} }
    version (all) { shared:       static void label_s() {} }
    version (all) { shared const: static void label_sc() {} }
    version (all) { shared inout: static void label_sw() {} }

    static assert(is(typeof(label_c) == F));
    static assert(is(typeof(label_i) == F));
    static assert(is(typeof(label_w) == F));
    static assert(is(typeof(label_s) == F));
    static assert(is(typeof(label_sc) == F));
    static assert(is(typeof(label_sw) == F));
}
