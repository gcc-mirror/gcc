/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-options "-Os -w -mno-ll64" } */

/* This test studies the use of anchors and tls symbols. */

struct a b;
struct a {
  long c;
  long d
} e() {
  static __thread struct a f;
  static __thread g;
  g = 5;
  h();
  if (f.c)
    g = g & 5;
  f = b;
}
