/* PR target/42564 */
/* This used to ICE on the SPARC because of an unrecognized TLS pattern.  */

/* { dg-do compile } */
/* { dg-options "-O -fPIC" } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target fpic } */

extern void *memset(void *s, int c, __SIZE_TYPE__ n);

struct S1 { int i; };

struct S2
{
  int ver;
  struct S1 s;
};

static __thread struct S2 m;

void init(void)
{
  memset(&m.s, 0, sizeof(m.s));
}
