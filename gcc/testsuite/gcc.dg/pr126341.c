/* { dg-do compile { target bitint575 } } */
/* { dg-options "-fnon-call-exceptions -ftree-coalesce-vars" } */
/* { dg-prune-output "division by zero" } */

/* PR middle-end/126341 */

#define N 0x200000000uwb
#define DIV0 (1 / 0)

struct {
  char j[8];
} k;
char l;
int m, p, q, r;
long n;
_Complex o;
int s;
_BitInt (256) aa;

void
foo (_BitInt (8) t)
{
  int ah, ai, aj = 1, ak, al = 1, am = 1, an = 1, ao = 1, ap = 1, aq = DIV0;
  int ar = 1, as = 1, at, au = DIV0;
  int av = 1;
  _BitInt (7) c = ak--;
  char aw = 1, ax = 1, ay = 1, az = 1, a = 1, d = 1, e = 1;
  int bfbg = 1, bh = 1, bi = p += ai -= __sync_add_and_fetch (&s, 1);
  int bj;
  int bk, blbm = 1, bt = 1, bu = 1, bv = __atomic_or_fetch (&aa, 1, 1);
  int bn = 1, bobp = 1, br = 1, bs = 1;
  unsigned b, cc;
  char cf, ce;

  if (bj)
    goto ca;
  b = 1, cc = 1;
  for (;;);
  ce = q;
  cf = o ? 1
    :
    __builtin_stdc_rotate_left (__builtin_stdc_rotate_left
				(c - 0x3bfuwb, br), n & 1) ? 3 : 3;
  1 +
    az & 1 | N << -az & m | ((az & 1 | N) << -az & 1 ? 0 : 7u) & 1 |
    ((az & 1 | N) - az & m | az & 1 | N) < 7 > -c & 1;
ca:
  s %= __builtin_stdc_rotate_left (cc, ah);
  at /= r ^= ({ __auto_type cg = 0; __auto_type ch = 0; 0; });

  long ci = bk % cf;
  _BitInt (8) cj = l ? t : k.j[3];
}
