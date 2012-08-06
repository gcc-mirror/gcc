/* Test the new (Fujitsu-compatible) __MDPACKH() interface.  */
/* { dg-do run } */
extern void exit (int);
extern void abort (void);

unsigned short x[] = { 0x8765, 0x1234, 0x2222, 0xeeee };

int
main ()
{
  if (__MDPACKH (x[0], x[1], x[2], x[3]) != 0x876522221234eeeeULL)
    abort ();
  if (__MDPACKH (0x1111, 0x8001, 0xeeee, 0x7002) != 0x1111eeee80017002ULL)
    abort ();
  exit (0);
}
