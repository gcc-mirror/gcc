/* This used to ICE due to a reload bug on s390*.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */


typedef struct test
{
  unsigned short pad;
  unsigned char type[6];
} t;

extern void set (t *a, t *b, t *c, t *d, t *e, t *f, t *g, t *h, 
		 t *i, t *j, t *k, t *l, t *m, t *n, t *o, t *p);
extern void use (t a, t b, t c, t d, t e, t f, t g, t h,
		 t i, t j, t k, t l, t m, t n, t o, t p);
void test (void)
{
    t a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p;
    set (&a, &b, &c, &d, &e, &f, &g, &h, &i, &j, &k, &l, &m, &n, &o, &p);
    use (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p);
}
