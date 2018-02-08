// { dg-do compile }
// { dg-additional-options "-w" }

void a(const char *, int, const char *, const char *);
template <typename b> void c(b);
struct d {
    long e;
    template <typename> union f;
    template <typename h> union f<h *> {
	f(h *i) : j(i) {}
	h *j;
	long bits;
    };
    static int k(volatile long &i) { return *(int *)f<volatile long *>(&i).bits; }
    typedef long g;
    operator g() volatile {
	int l = k(e);
	c(l);
    }
};
struct : d {
	 } m, n;
bool o;
void p() { (o ? m : n) ? (void)0 : a("", 5, "", ""); }

