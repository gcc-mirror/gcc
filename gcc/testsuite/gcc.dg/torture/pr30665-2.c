/* PR target/30665: bug in cris.md peephole2 condition.
   Original reduced testcase (fails on 3.2.1 derivate, not on trunk).  */
/* { dg-do run } */

extern void abort (void);
extern void exit (int);

struct t
{
  unsigned int a : 12;
  unsigned int b  : 12;
  unsigned int dummy1 : 8;
};

struct area
{
  int xa;
  int xb;
};

struct c
{
  struct area ii;
};

static struct c c;

void  __attribute__ ((__noinline__)) g(int a)
{
  if (a != 79)
    abort ();
}

void  __attribute__ ((__noinline__)) h(struct t tt)
{
  if (tt.a != 20 || tt.b != 79)
    abort ();
}

void __attribute__ ((__noinline__)) s(void);

int main(int argc, char **argv)
{
  c.ii.xa = 20;
  c.ii.xb = 79;

  s();

  exit (0);
}

void __attribute__ ((__noinline__)) s(void)
{
  struct t ii_x = { .a = c.ii.xa, .b = c.ii.xb };
  g(c.ii.xb);
  h(ii_x);
}
