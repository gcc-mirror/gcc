/* Test function argument passing.  This was written when correcting
   a deviation from the ABI on SPARC64 between 3.3 and 3.4.  */

extern void union_by_value_1_x (void);
extern void exit (int);
int fails;

int
main ()
{
  union_by_value_1_x ();
  exit (0);
}
