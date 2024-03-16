extern void *malloc(__SIZE_TYPE__);
extern void abort(void);
extern void free(void *);

typedef struct SEntry
{
  unsigned char num;
} TEntry;

typedef struct STable
{
  TEntry data[2];
} TTable;

TTable *init ()
{
  return malloc(sizeof(TTable));
}

void
expect_func (int a, unsigned char *b) __attribute__ ((noinline));

static inline void
inlined_wrong (TEntry *entry_p, int flag);

void
inlined_wrong (TEntry *entry_p, int flag)
{
  unsigned char index;
  entry_p->num = 0;

  if (flag == 0)
    abort();

  for (index = 0; index < 1; index++)
    entry_p->num++;

  if (!entry_p->num)
    {
      abort();
    }
}

void
expect_func (int a, unsigned char *b)
{
  if (__builtin_abs ((a == 0)))
    abort ();
  if (__builtin_abs ((b == 0)))
    abort ();
}

int
main ()
{
  unsigned char index = 0;
  TTable *table_p = init();
  TEntry work;

  inlined_wrong (&(table_p->data[1]), 1);
  expect_func (1, &index);
  inlined_wrong (&work, 1);

  free (table_p);

  return 0;
}

