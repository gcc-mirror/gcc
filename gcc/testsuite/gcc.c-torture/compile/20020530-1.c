/* PR optimization/6822 */

extern unsigned char foo1 (void);
extern unsigned short foo2 (void);

int bar1 (void)
{
  unsigned char q = foo1 ();
  return (q < 0x80) ? 64 : 0;
}

int bar2 (void)
{
  unsigned short h = foo2 ();
  return (h < 0x8000) ? 64 : 0;
}
