/* PR rtl-optimization/17825 */
#ifdef __i386__
register unsigned int reg __asm ("esi");
#elif defined __x86_64__
register unsigned int reg __asm ("r14");
#else
unsigned int reg;
#endif

struct S
{
  unsigned int h[8];
} *b;
unsigned int c;
void foo (int);

void
bar (void)
{
  unsigned int j, k, l, m;

  j = (reg & 0xffff) | ((b->h[2] & 0xffff) << 16);
  k = c & 0xffff;
  if (k == 0)
    foo (0);
  l = (j / k) & 0xffff;
  m = (j % k) & 0xffff;
  reg = (reg & 0xffff0000) | l;
  b->h[2] = (b->h[2] & 0xffff0000) | m;
}
