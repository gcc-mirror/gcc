/* Test compatibility of vector types: layout between separately-compiled
   modules, parameter passing, and function return.  This test uses
   vectors of integer values.  */

extern void vector_1_x (void);
extern void exit (int);
int fails;

int
main ()
{
  vector_1_x ();
  exit (0);
}
