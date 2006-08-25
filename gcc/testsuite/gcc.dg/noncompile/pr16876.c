/* { dg-options "-O -finline-functions" } */

static void g();
struct bigstack {
   char space[4096];
};


void f() {
    g(0); /* { dg-error "incompatible type for argument 1 of 'g'" } */
}

static void g(struct bigstack bstack) {
     g(bstack);
}
