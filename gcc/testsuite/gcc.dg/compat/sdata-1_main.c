/* Check that sdata qualification doesn't produce out-of-range relocations
   and that compilers agree on the way these declarations are handled.  */

extern void sdata_1_x (void);
extern void exit (int);

int
main ()
{
  sdata_1_x ();
  exit (0);
}
