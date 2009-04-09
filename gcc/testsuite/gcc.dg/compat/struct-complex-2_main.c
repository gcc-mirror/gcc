/* { dg-options "-O" } */

#ifdef __x86_64__
/* Test function argument passing.  PR target/39678.  */

extern void struct_complex_2_x (void);
extern void exit (int);

int
main ()
{
  struct_complex_2_x ();
  exit (0);
}
#else
int
main ()
{
  return 0;
}
#endif
