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
  short ee;
};

struct ZA
{
  struct SA s;
  short q;
};

struct AB
{
  unsigned int bx;
  unsigned long by;
  unsigned long bz;
};

struct SB
{
  int p;
  struct AB arr[2];
  short ee;
};

struct ZB
{
  struct SB s;
  short q;
};

void __attribute__((noinline))
getb (struct SB *d, struct ZB *p)
{
  struct SB tmp = p->s;
  *d = tmp;
}
