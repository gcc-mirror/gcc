/* { dg-do run } */

extern void abort (void);
extern void exit (int);

#define small   __attribute__((mode(QI))) int
int main()
{
  int x, y = 0x400;

  x = (small) y;                                /* { dg-bogus "ignored" } */
  if (sizeof (small) != sizeof (char))          /* { dg-bogus "ignored" } */
    abort ();
  if (sizeof (x) != sizeof (char) && x == y)
    abort ();
  return 0;
}
