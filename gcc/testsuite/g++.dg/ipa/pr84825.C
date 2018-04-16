/* PR ipa/84658 */
/* { dg-options "-O3 --param early-inlining-insns=0 -fno-guess-branch-probability" } */

struct a;
struct b;
struct c {
  virtual a *d(b *);
};
struct a {
  virtual a e();
};
struct f {
  virtual ~f();
};
struct g : f, a {};
struct b : c, virtual g {
  b *d(b *h) { return h; }
} i;
