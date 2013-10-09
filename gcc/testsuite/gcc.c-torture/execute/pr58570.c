#pragma pack(1)
struct S
{
  int f0:15;
  int f1:29;
};

int e = 1, i;
static struct S d[6];

int
main (void)
{
  if (e)
    {
      d[i].f0 = 1;
      d[i].f1 = 1;
    }
  if (d[0].f1 != 1)
    __builtin_abort ();
  return 0;
}
