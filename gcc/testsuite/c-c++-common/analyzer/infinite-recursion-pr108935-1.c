typedef struct {
    unsigned idx;
    int vals[512];
} foo_t;

int ended(foo_t* f) {
    return f->idx >= 512;
}
unsigned foo(foo_t* f) {
    if (ended(f)) {
        return f->idx;
    }
    do {
        f->idx++;
    } while(!ended(f) && !f->vals[f->idx]);
    return foo(f); /* { dg-bogus "infinite recursion" } */
}
