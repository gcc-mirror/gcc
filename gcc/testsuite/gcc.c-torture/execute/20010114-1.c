/* Origin: PR c/1540 from Mattias Lampe <lampe@tu-harburg.de>,
   adapted to a testcase by Joseph Myers <jsm28@cam.ac.uk>.
   GCC 2.95.2 fails, CVS GCC of 2001-01-13 passes.  */
extern void abort (void);
extern void exit (int);

int
main (void)
{
  int array1[1] = { 1 };
  int array2[2][1]= { { 1 }, { 0 } };
  if (array1[0] != 1)
    abort ();
  exit (0);
}
