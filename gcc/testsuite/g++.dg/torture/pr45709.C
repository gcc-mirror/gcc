// { dg-do compile }

struct Region {
    int storage[4];
    int count;
};
static inline Region subtract(int lhs)
{
  Region reg;
  int* storage = reg.storage;
  if (lhs > 0)
    storage++;
  reg.count = storage - reg.storage;
  return reg;
}
void bar(int a)
{
  const Region copyBack(subtract(a));
}
