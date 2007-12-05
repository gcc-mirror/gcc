/* PR middle-end/34337 */

extern void abort (void);

int
foo (int x)
{
  return ((x << 8) & 65535) | 255;
}

int
main (void)
{
  if (foo (0x32) != 0x32ff || foo (0x174) != 0x74ff)
    abort ();
  return 0;
}
