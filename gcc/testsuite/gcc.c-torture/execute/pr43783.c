typedef __attribute__((aligned(16)))
struct {
  unsigned long long w[3];
} UINT192;

UINT192 bid_Kx192[32];

extern void abort (void);

int main()
{
  int i = 0;
  unsigned long x = 0;
  for (i = 0; i < 32; ++i)
    bid_Kx192[i].w[1] = i == 1;
  for (i = 0; i < 32; ++i)
    x += bid_Kx192[1].w[1];
  if (x != 32)
    abort ();
  return 0;
}
