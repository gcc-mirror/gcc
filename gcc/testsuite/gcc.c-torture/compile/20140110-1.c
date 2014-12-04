typedef long unsigned int size_t;
struct RangeMapRange {
  unsigned fromMin;
  unsigned fromMax;
  unsigned toMin;
};
void reserve1(void);
void f(struct RangeMapRange *q1, size_t t)
{
  const struct RangeMapRange *q2 = q1 + t;
  size_t n = q2 - q1;
  if (n > 0)
    reserve1();
}
