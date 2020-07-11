// { dg-do compile }
// { dg-options "-O2 -fsanitize=address" }

struct a {
    int b;
} * c;
struct d {
    d *e;
};
struct f {
    bool done;
    d *g;
};
int h;
int i(f *j) {
    if (j->g) {
	j->g = j->g->e;
	return h;
    }
    j->done = true;
    return 0;
}
void k(bool j) { c->b = j; }
void l() {
    f a;
    for (; !(&a)->done; i(&a))
      k(true);
}
