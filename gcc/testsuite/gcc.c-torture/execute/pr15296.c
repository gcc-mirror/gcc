/* PR optimization/15296.  The delayed-branch scheduler caused code that
   SEGV:d for CRIS; a register was set to -1 in a delay-slot for the
   fall-through code, while that register held a pointer used in code at
   the branch target.  */

typedef __INTPTR_TYPE__ intptr_t;
typedef intptr_t W;
union u0
{
  union u0 *r;
  W i;
};
struct s1
{
  union u0 **m0;
  union u0 m1[4];
};

void f (void *, struct s1 *, const union u0 *, W, W, W)
     __attribute__ ((__noinline__));
void g (void *, char *) __attribute__ ((__noinline__));

void
f (void *a, struct s1 *b, const union u0 *h, W v0, W v1, W v4)
{
  union u0 *e = 0;
  union u0 *k = 0;
  union u0 **v5 = b->m0;
  union u0 *c = b->m1;
  union u0 **d = &v5[0];
l0:;
  if (v0 < v1)
    goto l0;
  if (v0 == 0)
    goto l3;
  v0 = v4;
  if (v0 != 0)
    goto l3;
  c[0].r = *d;
  v1 = -1;
  e = c[0].r;
  if (e != 0)
    g (a, "");
  k = e + 3;
  k->i = v1;
  goto l4;
l3:;
  c[0].i = v0;
  e = c[1].r;
  if (e != 0)
    g (a, "");
  e = c[0].r;
  if (e == 0)
    g (a, "");
  k = e + 2;
  k->r = c[1].r;
l4:;
}

void g (void *a, char *b) { abort (); }

int
main ()
{
  union u0 uv[] = {{ .i = 111 }, { .i = 222 }, { .i = 333 }, { .i = 444 }};
  struct s1 s = { 0, {{ .i = 555 }, { .i = 0 }, { .i = 999 }, { .i = 777 }}};
  f (0, &s, 0, 20000, 10000, (W) uv);
  if (s.m1[0].i != (W) uv || s.m1[1].i != 0 || s.m1[2].i != 999
      || s.m1[3].i != 777 || uv[0].i != 111 || uv[1].i != 222
      || uv[2].i != 0 || uv[3].i != 444)
    abort ();
  exit (0);
}
