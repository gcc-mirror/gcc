/* { dg-options "-O" } */

#ifdef __x86_64__
/* Test function argument passing.  PR target/15301.  */

extern void union_m128_1_x (void);
extern void exit (int);

int
main ()
{
  union_m128_1_x ();
  exit (0);
}
#else
int
main ()
{
  return 0;
}
#endif
