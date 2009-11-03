// PR c++/36959
// We shouldn't have to emit fromSlotB just because we need shuf_BZZZ.
// { dg-options -O }
// { dg-final { scan-assembler-not "_ZL9fromSlotBv" } }

static inline int *fromSlotB(void)
{
  static int shuf_BZZZ = 1;
  return &shuf_BZZZ;
}

int *p;

int main(void)
{
  p = fromSlotB();
  return (*p != 1);
}

