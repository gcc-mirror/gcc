typedef struct
{
  int             baddr;
} mstruct_t;

static struct
{
  unsigned int    mapnum;
  mstruct_t       unused;
} mtab;

static mstruct_t *mactab = &mtab.unused;

int
main(void)
{
  int i;
  int addr;

  for (i=1; i <= mtab.mapnum; i++)
    if (addr < mactab[i].baddr)
      break;
  return 0;
}

