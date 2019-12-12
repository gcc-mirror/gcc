/* { dg-do run } */
/* { dg-options "-O2 -fipa-sra" } */


struct __attribute__((scalar_storage_order("little-endian"))) LE
{
  int i;
  int j;
};

struct __attribute__((scalar_storage_order("big-endian"))) BE
{
  int i;
  int j;
};

struct LE gle;
struct BE gbe;

#define VAL 0x12345678

void __attribute__((noipa))
fill (void)
{
  gle.i = VAL;
  gle.j = 0xdeadbeef;
  gbe.i = VAL;
  gbe.j = 0x11223344;
}

static int __attribute__((noinline))
readLE (struct LE p)
{
  return p.i;
}

static int __attribute__((noinline))
readBE (struct BE p)
{
  return p.i;
}

int
main (int argc, char *argv[])
{
  int r;
  fill ();

  r = readLE (gle);
  if (r != VAL)
    __builtin_abort ();
  r = readBE (gbe);
  if (r != VAL)
    __builtin_abort ();

  return 0;
}
