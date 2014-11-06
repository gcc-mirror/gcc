/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options " -O -fno-guess-branch-probability -fpeel-loops -freorder-blocks-and-partition -fschedule-insns2 -fsel-sched-pipelining -fselective-scheduling2 -ftree-pre" } */

struct intC
{
  short x;
  short y;
};

void Get(void);

int size_x;

struct
{
  int *depot_table;
  struct intC *ti;
  int size;
} dummy;

static inline int
GetRotatedTileFromOffset (int *a, struct intC tidc)
{
  if (!*a)
    Get ();
  switch (*a)
    {
    case 0:
      return (tidc.y << size_x) + tidc.x;
    case 1:
      return tidc.y + (dummy.size - tidc.x) * size_x;
    case 2:
      return tidc.x + (dummy.size - tidc.y) * size_x;
    case 3:
      return (dummy.size - tidc.x);
    }
  return 0;
}

int
GetHangarNum (int *a, int i)
{
  while (dummy.size)
    if (GetRotatedTileFromOffset (a, dummy.ti[i]))
      return *dummy.depot_table;
  return 0;
}
