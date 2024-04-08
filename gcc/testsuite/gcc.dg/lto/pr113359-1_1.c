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
getb (struct SB *d, struct ZB *p)
{
  struct SB tmp = p->s;
  *d = tmp;
}
