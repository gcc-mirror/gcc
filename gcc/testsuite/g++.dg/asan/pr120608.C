// PR middle-end/120608
// { dg-do compile { target musttail } }
// { dg-options "-fsanitize=address" }
// { dg-skip-if "" { *-*-* } { "*" } { "-O0" } }

struct A;
struct B { unsigned long b; };
typedef const char *(*F) (void *u, const char *v, void *w, const struct A *x,
			  unsigned long y, struct B z);
struct A { F a; };

const char *
foo (void *u, const char *v, void *w, const struct A *x, unsigned long y,
     struct B z)
{
  [[gnu::musttail]] return x->a (u, v, w, x, y, z);
}
