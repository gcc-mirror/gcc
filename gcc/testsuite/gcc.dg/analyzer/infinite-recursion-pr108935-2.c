typedef struct {
    unsigned done;
} foo_t;

unsigned foo(foo_t* f) {
    if (f->done) {
        return f->done;
    }
    f->done = 1;
    return foo(f); /* { dg-bogus "infinite recursion" } */
}

int main() {
    foo_t f = (foo_t){
        .done = 0,
    };
    foo(&f);
}
