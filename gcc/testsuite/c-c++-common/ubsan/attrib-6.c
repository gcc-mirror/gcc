/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

static void __attribute__((no_sanitize("foobar")))
foo (void) { /* { dg-warning "attribute directive ignored" } */
}

static void __attribute__((no_sanitize("address,undefined")))
foo2 (void) {
}

static void __attribute__((no_sanitize("address", "undefined")))
foo3 (void) {
}

static void __attribute__((no_sanitize("address", "address", "")))
foo4 (void) {
}

static void __attribute__((no_sanitize("address", "address", "address,address")))
foo5 (void) {
}

static void __attribute__((no_sanitize("address", "address,kernel-address,thread,leak,undefined,vptr,shift,integer-divide-by-zero,unreachable,vla-bound,null,return,signed-integer-overflow,bounds,bounds-strict,alignment,object-size,float-divide-by-zero,float-cast-overflow,nonnull-attribute,returns-nonnull-attribute,bool,enum")))
foo6 (void) {
}
