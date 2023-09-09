typedef __SIZE_TYPE__ size_t;

extern size_t fread (void *, size_t, size_t, void *);

struct ring
{
  char buf[1024];
};

int
test_one_large_item (void *fp)
{
  struct ring ring;
  int ret;

  ret = fread(&ring, sizeof(ring), 1, fp);

  if (ret != 1)
    return 1;

  if (ring.buf[0] > 1) /* { dg-bogus "use of uninitialized value" } */
    return 2;
  return 3;
}

int
test_many_small_items (void *fp)
{
  struct ring ring;
  int ret;

  ret = fread(&ring, 1, sizeof(ring), fp);

  if (ret != sizeof(ring))
    return 1;

  if (ring.buf[0] > 1) /* { dg-bogus "use of uninitialized value" } */
    return 2;
  return 3;
}
