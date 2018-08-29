/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */

typedef struct {
    struct {
	char a;
    } b;
} c;

int d, f;
c *e;

extern void i(void);
extern void sejtmp () __attribute__((returns_twice));

void g(void)
{
  c *h = e;
  if (f)
    {
      i();
      h--;
      if (d)
	if (h->b.a)
	  i();
    }
  if (h->b.a)
    sejtmp();
  e = h;
}
