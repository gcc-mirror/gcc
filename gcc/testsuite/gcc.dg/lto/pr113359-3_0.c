/* { dg-lto-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-lto-options {{-O2 -flto -fno-strict-aliasing -fno-ipa-cp  --disable-tree-esra }} }  */

#define CI 0xdeadbeef
#define CL1 0xdeaddead1234beef
#define CL2 0xdead1234deadbeef

struct AA
{
  unsigned int ax;
  unsigned long ay;
  unsigned long az;
};

struct SA
{
  int p;
  struct AA arr[2];
};

struct ZA
{
  struct SA s;
  short q;
};

struct AB
{
  unsigned long bx;
  unsigned int by;
  unsigned long bz;
};

struct SB
{
  int p;
  struct AB arr[2];
};

struct ZB
{
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
  ga.s.arr[0].ax = CI;
  ga.s.arr[0].ay = CL1;
  ga.s.arr[0].az = CL2;
  ga.s.arr[1].ax = CI;
  ga.s.arr[1].ay = CL1;
  ga.s.arr[1].az = CL2;

  gb.s.arr[0].bx = CL1;
  gb.s.arr[0].by = CI;
  gb.s.arr[0].bz = CL2;
  gb.s.arr[1].bx = CL1;
  gb.s.arr[1].by = CI;
  gb.s.arr[1].bz = CL2;
}

int
main (int argc, char **argv)
{
  init();
  struct SA a;
  geta (&a, &ga);

  if (a.arr[0].ax != CI)
    __builtin_abort ();
  if (a.arr[0].ay != CL1)
    __builtin_abort ();
  if (a.arr[0].az != CL2)
    __builtin_abort ();
  if (a.arr[1].ax != CI)
    __builtin_abort ();
  if (a.arr[1].ay != CL1)
    __builtin_abort ();
  if (a.arr[1].az != CL2)
    __builtin_abort ();

  struct SB b;
  getb (&b, &gb);

  if (b.arr[0].bx != CL1)
    __builtin_abort ();
  if (b.arr[0].by != CI)
    __builtin_abort ();
  if (b.arr[0].bz != CL2)
    __builtin_abort ();
  if (b.arr[1].bx != CL1)
    __builtin_abort ();
  if (b.arr[1].by != CI)
    __builtin_abort ();
  if (b.arr[1].bz != CL2)
    __builtin_abort ();

  return 0;
}
