/* { dg-lto-options {{ -O1 -flto -flto-partition=1to1 }} } */
/* { dg-lto-do run } */

/* Test that ipa-reference notice that get_val will not change since do_nothing does not
   modify anything.  This needs streaming cross file boundary summaries.  */
extern int get_val (void);
extern int set_val (void);
extern void do_nothing (void);
void abort (void);
int
main()
{
  int a;
  int b;
  set_val ();
  a = get_val ();
  do_nothing();
  b = get_val ();
  if (a==b)
    {
      if (!__builtin_constant_p (a==b))
	abort ();
      return 0;
    }
  else
    abort ();
}
