/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O -fno-guess-branch-probability -fpeel-loops -freorder-blocks-and-partition -fschedule-insns2 -fsel-sched-pipelining -fselective-scheduling2" } */
struct intC
{
  short x;
  short y;
};

int size_x;

static inline int
TileDiffXY (int x, int y)
{
  return (y * size_x) + x;
}

struct HangarTileTable
{
  struct intC ti;
  int hangar_num;
};

struct AirportSpec
{
  struct HangarTileTable *depot_table;
  int size;
};

void Get ();
struct AirportSpec dummy;

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
      return TileDiffXY (tidc.y, dummy.size - tidc.x);
    case 2:
      return TileDiffXY (tidc.x, dummy.size - tidc.y);
    case 3:
      return TileDiffXY (dummy.size - 1, tidc.x);
    }
}

int
GetHangarNum (int *a)
{
	int i;
  for (i = 0; i < dummy.size; i++)
    if (GetRotatedTileFromOffset (a, dummy.depot_table[i].ti))
      return dummy.depot_table[i].hangar_num;
}
