/* Origin: PR c/364 from and@genesyslab.com, very much reduced to a
   testcase by Joseph Myers <jsm28@cam.ac.uk>.

   The initializer of z is a valid address constant, and GCC 2.95.2
   accepts it as such.  CVS GCC as of 2001-01-13 rejects it, but accepts
   it if y is changed to x in the initializer.  */

struct {
  struct {
    int x;
    int y;
  } p;
} v;

int *z = &((&(v.p))->y);
