struct bitmap
{
  int min;
  int max;
  int *vec;
};

int bitmap_create(struct bitmap *bm, int min, int max)
{
  int sz;

  sz = (max / sizeof(int)) + 1;

  bm->min = min;
  bm->max = max;
  bm->vec = __builtin_calloc(sz, sizeof(int));
  if (!bm->vec)
    return (-12);
  return 0; /* { dg-bogus "leak" } */
}
