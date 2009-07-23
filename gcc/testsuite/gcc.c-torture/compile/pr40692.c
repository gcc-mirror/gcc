/* PR middle-end/40692 */

#define M1(x) (((x) & 0x00000002) ? 0x2 : ((x) & 0x1))
#define M2(x) (((x) & 0x0000000c) ? M1 ((x) >> 2) << 2 : M1 (x))
#define M3(x) (((x) & 0x000000f0) ? M2 ((x) >> 4) << 4 : M2 (x))
#define M4(x) (((x) & 0x0000ff00) ? M3 ((x) >> 8) << 8 : M3 (x))
#define M5(x) (((x) & 0xffff0000) ? M4 ((x) >> 16) << 16 : M4 (x))

struct A { char e; char f; };

long
foo (void)
{
  return M5 (4096UL - (long) &((struct A *) 0)->f);
}
