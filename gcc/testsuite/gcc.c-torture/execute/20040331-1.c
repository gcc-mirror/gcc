/* PR c++/14755 */
extern void abort (void);
extern void exit (int);

int
main (void)
{
#if __INT_MAX__ >= 2147483647
  struct { int count: 31; } s = { 0 };
  while (s.count--)
    abort ();
#elif __INT_MAX__ >= 32767
  struct { int count: 15; } s = { 0 };
  while (s.count--)
    abort ();
#else
  /* Don't bother because __INT_MAX__ is too small.  */
#endif
  exit (0);
}
