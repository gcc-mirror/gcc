/* PR ipa/110705 */

/* We used to ICE in gimplify_modify_expr due to
   the gimplifier not handling VCE between an struct
   and an integer type.  */

struct a {
  long b;
};
union d {
  struct a b;
  int e;
}v;
long c;
int f;
static void g(union d h, long i) {
  while (1)
    switch (c)
    case 4:
      if (h.e)
        c = 4;
}
void j(union d *h) {
  if (f)
    g(*h, h->b.b);
}
void k() { union d *h = &v; j(h); }
