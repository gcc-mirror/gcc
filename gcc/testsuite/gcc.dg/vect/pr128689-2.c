/* { dg-do run } */

struct
{
  unsigned char ring[256];
  unsigned char after[4096];
} g;

int __attribute__ ((noipa))
find (unsigned char i, unsigned char key)
{
  int n = 0;
  while (g.ring[i] != key) /* i wraps 255 -> 0 */
    {
      i++;
      n++;
    }
  return n;
}

int
main (void)
{
  g.ring[10] = 1;
  g.after[100] = 1;
  int n = find (200, 1); /* 200..255, 0..10 -> 66 */
  if (n != 66)
    __builtin_abort ();
}
