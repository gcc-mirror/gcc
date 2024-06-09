#define CI 0xdeadbeef
#define CL1 0xdeaddead1234beef
#define CL2 0xdead1234deadbeef

struct SA
{
  unsigned int ax;
  unsigned long long ay;
  unsigned long long az;
};

struct SB
{
  unsigned int bx;
  unsigned long long by;
  unsigned long long bz;
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
