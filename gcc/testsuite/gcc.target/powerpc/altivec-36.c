/* PR target/70296 */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -std=gnu11" } */

#define c(x) x
#define f(x)
#define i int
#define k
typedef int vector;
typedef vector int V;
vector int a;
vector b;
vector c(int) d;
vector c(e);
vector c;
vector f(int) int g;
vector f(int) h;
vector i j;
vector k int l;
vector k m;
#define int(x) x
vector int n;
vector int(int) o;
vector int(r);
#undef int

void
foo ()
{
  V *p;
  p = &a;
  p = &d;
  p = &g;
  p = &j;
  p = &l;
  p = &n;
  p = &o;
  int *q;
  q = &b;
  q = &e;
  q = &c;
  q = &h;
  q = &m;
  q = &r;
}
