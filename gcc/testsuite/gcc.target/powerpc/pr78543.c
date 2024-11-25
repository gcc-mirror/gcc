/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O1 -std=gnu17" } */
/* { dg-require-effective-target powerpc_vsx } */

typedef long a;
enum c { e, f, g, h, i, ab } j();
int l, n, o, p;
a q, r;
void *memcpy();
void b();
static int k(int *s) {
  int m;
  if (j(&m))
    *s = m;
  return !0;
}
void d(char s) {
  int af[4];
  int ag;
  enum c ah;
  char ai[24 << 11];
  unsigned aj;
  if (!k(&aj))
    goto ak;
  for (;;) {
    if (!k(&ag))
      goto ak;
    switch (ah) {
    case e:
      b("");
      b("bad length %d for GUID in fileinfo v%u for \"%s\"");
    case i:
      b("bad length %d for TTH in fileinfo v%u for \"%s\"", aj);
    case ab:
      if (ag % 24)
        b("for \"%s\"", s);
    case f:
      if (20 == ag)
      case h:
        if (20 == ag)
          o = 0;
      break;
    case g:
      memcpy(af, ai, sizeof af);
      b();
      if (p) {
        a al, am;
        r = al << 2 | am;
        n = af[2];
        al = n;
        l = __builtin_bswap32(af[3]);
        am = q = n | l;
      }
    default:
      b("%s0 unhandled field ID %u 0", __func__);
    }
  }
ak:;
}
