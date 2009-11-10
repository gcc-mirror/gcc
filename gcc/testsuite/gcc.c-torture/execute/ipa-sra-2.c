struct big
{
  int data[1000000];
};

struct small
{
  int data[10];
};

union both
{
  struct big big;
  struct small small;
};

extern void *calloc (__SIZE_TYPE__, __SIZE_TYPE__);
extern void free (void *);

static int __attribute__((noinline))
foo (int fail, union both *agg)
{
  int r;
  if (fail)
    r = agg->big.data[999999];
  else
    r = agg->small.data[0];
  return r;
}

int main (int argc, char *argv[])
{
  union both *agg = calloc (1, sizeof (struct small));
  int r;

  r = foo ((argc > 2000), agg);

  free (agg);
  return r;
}
