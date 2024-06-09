/* { dg-lto-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-lto-options {{-O2 -flto -fno-strict-aliasing -fno-ipa-cp  --disable-tree-esra }} }  */

#define CI 0xdeadbeef
#define CL1 0xdeaddead1234beef
#define CL2 0xdead1234deadbeef

struct SA
{
  unsigned int ax;
  unsigned long ay;
  unsigned long az;
};

struct SB
{
  unsigned long bx;
  unsigned int by;
  unsigned long bz;
};

struct ZA
{
  int p;
  struct SA s;
  short q;
};

struct ZB
{
  int p;
  struct SB s;
  short q;
};

void __attribute__((noinline))
geta (struct SA *d, struct ZA *p)
{
  struct SA tmp = p->s;
  *d = tmp;
}

void getb (struct SB *d, struct ZB *p);

struct ZA ga;
struct ZB gb;

void __attribute__((noipa))
init (void)
{
  ga.s.ax = CI;
  ga.s.ay = CL1;
  ga.s.az = CL2;

  gb.s.bx = CL1;
  gb.s.by = CI;
  gb.s.bz = CL2;
}

int
main (int argc, char **argv)
{
  init();
  struct SA a;
  geta (&a, &ga);

  if (a.ax != CI)
    __builtin_abort ();
  if (a.ay != CL1)
    __builtin_abort ();
  if (a.az != CL2)
    __builtin_abort ();

  struct SB b;
  getb (&b, &gb);

  if (b.bx != CL1)
    __builtin_abort ();
  if (b.by != CI)
    __builtin_abort ();
  if (b.bz != CL2)
    __builtin_abort ();

  return 0;
}
